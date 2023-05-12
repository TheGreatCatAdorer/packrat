use std::{collections::HashMap, hash::Hash, iter::repeat};

pub struct Parser(Vec<u32>, Vec<fn(char) -> bool>);
#[derive(Clone)]
pub struct ParseTree(Vec<u32>);
pub struct MemoTable(HashMap<(u32, usize), Option<(ParseTree, usize)>>);

pub enum Grammar<T> {
    End,
    Ref(T),
    Seq(Vec<Grammar<T>>),
    Any(Vec<Grammar<T>>),
    All(Vec<Grammar<T>>),
    Char(fn(char) -> bool),
    Next,
    Memo(Box<Grammar<T>>),
    Word(String),
    Space,
    Many0(Box<Grammar<T>>),
    Many1(Box<Grammar<T>>),
}

pub enum ConvError<'a> {
    NoProduction(&'a str),
    TooLarge,
}

pub fn make_parser<'a>(grammar: Vec<(&'a str, Grammar<&'a str>)>) -> Result<Parser, ConvError> {
    match lower_grammar(grammar) {
        Err(p) => Err(ConvError::NoProduction(p)),
        Ok(g) => g.try_into().map_err(|()| ConvError::TooLarge),
    }
}

pub fn lower_grammar<'a>(
    grammar: Vec<(&'a str, Grammar<&'a str>)>,
) -> Result<Vec<Grammar<u32>>, &'a str> {
    let mut map = HashMap::new();
    for (i, (label, _)) in grammar.iter().enumerate() {
        map.insert(*label, i as u32);
    }
    let mut result = Vec::with_capacity(grammar.len());
    for (label, g) in grammar {
        if let Some(g) = replace_refs(g, &map) {
            result.push(g);
        } else {
            return Err(label);
        }
    }
    Ok(result)
}

fn replace_refs<T: Eq + Hash, U: Clone>(
    grammar: Grammar<T>,
    map: &HashMap<T, U>,
) -> Option<Grammar<U>> {
    match grammar {
        Grammar::End => Some(Grammar::End),
        Grammar::Next => Some(Grammar::Next),
        Grammar::Space => Some(Grammar::Space),
        Grammar::Char(f) => Some(Grammar::Char(f)),
        Grammar::Word(w) => Some(Grammar::Word(w)),
        Grammar::Ref(t) => map.get(&t).map(|u| Grammar::Ref(u.clone())),
        Grammar::Memo(t) => replace_refs(*t, map).map(|u| Grammar::Memo(Box::new(u))),
        Grammar::Many0(t) => replace_refs(*t, map).map(|u| Grammar::Many0(Box::new(u))),
        Grammar::Many1(t) => replace_refs(*t, map).map(|u| Grammar::Many1(Box::new(u))),
        Grammar::Seq(ts) => Some(Grammar::Seq(
            ts.into_iter()
                .map(|t| replace_refs(t, map))
                .collect::<Option<Vec<_>>>()?,
        )),
        Grammar::Any(ts) => Some(Grammar::Any(
            ts.into_iter()
                .map(|t| replace_refs(t, map))
                .collect::<Option<Vec<_>>>()?,
        )),
        Grammar::All(ts) => Some(Grammar::All(
            ts.into_iter()
                .map(|t| replace_refs(t, map))
                .collect::<Option<Vec<_>>>()?,
        )),
    }
}

fn grammar_cases(
    class: u32,
    cases: Vec<Grammar<u32>>,
    code: &mut Vec<u32>,
    tokens: &mut Vec<fn(char) -> bool>,
    to_memo: &mut Vec<(u32, u32)>,
) -> Result<(), ()> {
    code.push(class);
    code.push(cases.len().try_into().map_err(|_| ())?);
    let start = code.len();
    code.extend(repeat(0).take(cases.len()));
    for (i, g) in cases.into_iter().enumerate() {
        code[start + i] = code.len().try_into().map_err(|_| ())?;
        encode_grammar(g, code, tokens, to_memo)?;
    }
    code.push(Parser::TERM);
    Ok(())
}

fn encode_grammar(
    grammar: Grammar<u32>,
    code: &mut Vec<u32>,
    tokens: &mut Vec<fn(char) -> bool>,
    to_memo: &mut Vec<(u32, u32)>,
) -> Result<(), ()> {
    match grammar {
        Grammar::End => code.push(Parser::END),
        Grammar::Ref(g_i) => {
            to_memo.push((code.len().try_into().map_err(|_| ())?, g_i));
            code.push(0);
        }
        Grammar::Seq(cases) => grammar_cases(Parser::SEQ, cases, code, tokens, to_memo)?,
        Grammar::Any(cases) => grammar_cases(Parser::ANY, cases, code, tokens, to_memo)?,
        Grammar::All(cases) => grammar_cases(Parser::ALL, cases, code, tokens, to_memo)?,
        Grammar::Char(f) => {
            code.push(Parser::CHAR);
            code.push(tokens.len().try_into().map_err(|_| ())?);
            tokens.push(f);
        }
        Grammar::Next => {
            code.push(Parser::NEXT);
        }
        Grammar::Memo(g) => {
            code.push(Parser::MEMO);
            code.push(code.len().try_into().map_err(|_| ())?);
            encode_grammar(*g, code, tokens, to_memo)?;
        }
        Grammar::Word(w) => {
            code.push(Parser::WORD);
            code.push(w.chars().count() as u32);
            for c in w.chars() {
                code.push(c as u32);
            }
        }
        Grammar::Space => {
            code.push(Parser::SPACE);
        }
        Grammar::Many0(g) => {
            code.push(Parser::MANY0);
            code.push(code.len() as u32 + 1);
            encode_grammar(*g, code, tokens, to_memo)?;
        }
        Grammar::Many1(g) => {
            code.push(Parser::MANY1);
            code.push(code.len() as u32 + 1);
            encode_grammar(*g, code, tokens, to_memo)?;
        }
    }
    Ok(())
}

impl TryFrom<Vec<Grammar<u32>>> for Parser {
    type Error = ();
    fn try_from(value: Vec<Grammar<u32>>) -> Result<Self, ()> {
        let mut code = Vec::new();
        let mut tokens = Vec::new();
        let mut start_indices = Vec::with_capacity(value.len());
        let mut to_memo = Vec::new();
        for g in value {
            start_indices.push(code.len() as u32);
            encode_grammar(g, &mut code, &mut tokens, &mut to_memo)?;
        }
        for (p, g) in to_memo {
            code[p as usize] = start_indices[g as usize];
        }
        Ok(Parser(code, tokens))
    }
}

impl Parser {
    const END: u32 = 0;
    const SEQ: u32 = 1;
    const ANY: u32 = 2;
    const ALL: u32 = 3;
    const CHAR: u32 = 4;
    const NEXT: u32 = 5;
    const MEMO: u32 = 6;
    const WORD: u32 = 7;
    const SPACE: u32 = 8;
    const MANY0: u32 = 9;
    const MANY1: u32 = 10;
    const TERM: u32 = u32::MAX;
    pub fn run(&self, start: u32, string: &str) -> Option<(ParseTree, usize)> {
        self.run_from(start, string, 0, &mut MemoTable(HashMap::new()))
    }
    fn run_from(
        &self,
        state: u32,
        string: &str,
        mut pos: usize,
        table: &mut MemoTable,
    ) -> Option<(ParseTree, usize)> {
        let form = self.0[state as usize];
        if form == Self::END {
            (string.len() == pos).then_some((ParseTree(vec![ParseTree::END]), pos))
        } else if form == Self::SEQ {
            let mut state = state + 1;
            let mut res = vec![ParseTree::SEQ, 0];
            let mut count = 0;
            loop {
                let i = self.0[state as usize];
                state += 1;
                if i == 0 {
                    break;
                }
                let (mut t, p_next) = self.run_from(i, string, pos, table)?;
                pos = p_next;
                res.append(&mut t.0);
                count += 1;
            }
            res[1] = count;
            Some((ParseTree(res), pos))
        } else if form == Self::ANY || form == Self::ALL {
            let mut state = state + 1;
            let mut res = vec![ParseTree::ANY, 0];
            let mut count = 0;
            loop {
                let i = self.0[state as usize];
                state += 1;
                if i == Self::TERM {
                    break;
                }
                if let Some((mut t, p_next)) = self.run_from(i, string, pos, table) {
                    pos = p_next;
                    res.append(&mut t.0);
                    res[1] = count;
                    return Some((ParseTree(res), pos));
                }
                count += 1;
            }
            None
        } else if form == Self::CHAR {
            let pred = self.1[self.0[state as usize + 1] as usize];
            let mut rest = string[pos..].chars();
            let next = rest.next()?;
            if !pred(next) {
                return None;
            }
            let diff = next.len_utf8();
            let mut res = Vec::with_capacity(1 + diff);
            res.push(ParseTree::CHAR);
            res.push(next as u32);
            Some((ParseTree(res), pos + diff))
        } else if form == Self::NEXT {
            Some((ParseTree(Vec::new()), pos))
        } else if form == Self::MEMO {
            if let Some(res) = table.0.get(&(state, pos)) {
                res.as_ref().map(|(tree, pos)| (tree.clone(), *pos))
            } else {
                table.0.insert((state, pos), None);
                if let Some((t, p_next)) = self.run_from(state + 1, string, pos, table) {
                    table.0.insert((state, pos), Some((t.clone(), p_next)));
                    Some((t, p_next))
                } else {
                    None
                }
            }
        } else if form == Self::WORD {
            let len = self.0[state as usize + 1] as usize;
            let word = &self.0[state as usize + 2..state as usize + 2 + len];
            for (c, w) in string[pos..].chars().zip(word.iter().cloned()) {
                if c as u32 != w {
                    return None;
                }
            }
            Some((ParseTree(vec![ParseTree::WORD]), pos + word.len()))
        } else if form == Self::SPACE {
            let mut i = pos;
            while string.as_bytes()[i] == b' ' {
                i += 1;
            }
            (i != pos).then(|| (ParseTree(vec![ParseTree::SPACE]), i))
        } else if form == Self::MANY0 {
            let mut result = Vec::new();
            result.push(ParseTree::MANY);
            while let Some((mut t, p_next)) = self.run_from(state + 1, string, pos, table) {
                result.append(&mut t.0);
                pos = p_next;
            }
            result.push(ParseTree::TERM);
            Some((ParseTree(result), pos))
        } else if form == Self::MANY1 {
            let mut result = Vec::new();
            result.push(ParseTree::MANY);
            let Some((mut t, mut pos)) = self.run_from(state + 1, string, pos, table) else { return None; };
            result.append(&mut t.0);
            while let Some((mut t, p_next)) = self.run_from(state + 1, string, pos, table) {
                result.append(&mut t.0);
                pos = p_next;
            }
            result.push(ParseTree::TERM);
            Some((ParseTree(result), pos))
        } else {
            panic!("unknown state!");
        }
    }
}

impl ParseTree {
    const END: u32 = 0;
    const SEQ: u32 = 1;
    const ANY: u32 = 2;
    //const ALL: u32 = 3;
    const CHAR: u32 = 4;
    const WORD: u32 = 5;
    const SPACE: u32 = 6;
    const MANY: u32 = 7;
    const TERM: u32 = u32::MAX;
}
