/*
    lrparse - An LR parser generator for Rust
    Copyright (C) 2014  Jona Stubbe

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
extern crate lexer;
extern crate lookahead;

pub use lexer::{TokenContent, Identifier, Char, Lifetime, StringLiteral, Arrow, Scope, Equals, UnEqual, Other};
use std::io::{IoResult, IoError};
pub use nester::code_ref::{CodeReference, CodePoint};
use std::sync::Arc;
use self::lookahead::LookAhead;
use self::TokenTree::{Tok, Tree};

mod code_ref;

#[deriving(Clone,Show)]
pub enum TokenTree {
	Tok(TokenContent),
	Tree(char, Vec<Token>)
}

#[deriving(Clone)]
pub struct Token {
	pub content: TokenTree,
	pub ref_: CodeReference
}

impl Token {
	pub fn pretty_print_token<W: Writer>(&self, write: &mut W, indent: uint) -> IoResult<()> {
		match self.content {
			Tree(c @ _, ref t @ _) => {
				try!(write.write_char(match c {
					'}' => '{',
					')' => '(',
					']' => '[',
					_ => panic!()
				}));
				let mut newindent = indent;
				if c == '}' {
					try!(write!(write, "\n{st:\t<0$s}", indent + 1, st=""));
					newindent = indent + 1;
				}
				try!(t.iter().pretty_print(write, newindent, c == '}'));
				if c == '}' {
					try!(write!(write, "\n{st:\t<0$s}", indent, st=""));
				}
				write.write_char(c)
			},
			Tok(Char(c)) if c == '\\' || c == '\''
				=> write!(write, "'\\{}'", c),
			Tok(Char(c))
				=> write!(write, "'{}'", c),
			Tok(Lifetime(ref s))
				=> write!(write, "'{}", s),
			Tok(StringLiteral(ref s))
				=> write!(write, "\"{}\"", ::std::str::replace(
					::std::str::replace(s.as_slice(), "\\", "\\\\").as_slice(), "\"", "\\\"")),
			Tok(Identifier(ref s)) => write.write_str(s.as_slice()),
			Tok(Arrow) => write.write_str("=>"),
			Tok(Scope) => write.write_str("::"),
			Tok(Equals) => write.write_str("=="),
			Tok(UnEqual) => write.write_str("!="),
			Tok(Other(c)) => write.write_char(c)
		}
	}
	pub fn internal(t: TokenTree) -> Token {
		Token {
			content: t,
			ref_: CodeReference::internal()
		}
	}
}

pub trait PrettyPrint {
	fn pretty_print<W: Writer>(self, write: &mut W, indent: uint, break_on_comma: bool) -> IoResult<()>;
}

impl<'a, I: Iterator<&'a Token>> PrettyPrint for I {
	fn pretty_print<W: Writer>(mut self, write: &mut W, indent: uint, break_on_comma: bool) -> IoResult<()> {
		let mut just_had_text = false;
		for tok in self {
			match tok.content {
				Tok(Identifier(_)) | Tok(Lifetime(_)) | Tok(Char(_)) if just_had_text
					=> try!(write.write_char(' ')),
				Tok(Identifier(_)) | Tok(Lifetime(_)) | Tok(Char(_)) => { just_had_text = true; },
				_ => { just_had_text = false; }
			}
			try!(tok.pretty_print_token(write, indent));
			match tok.content {
				Tok(Other(',')) if break_on_comma => try!(write!(write, "\n{st:\t<0$s}", indent, st="")),
				Tok(Other(';')) => try!(write!(write, "\n{st:\t<0$s}", indent, st="")),
				_ => {}
			}
		}
		Ok(())
	}
}

impl ::std::fmt::Show for Token {
	fn fmt(&self, format: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::FormatError> {
		match self.content {
			Tok(ref x) => x.fmt(format),
			Tree(typ, ref tree) => match typ {
				'}' => format_args!(
					|args: &::std::fmt::Arguments|{ format.write_fmt(args) },
					"{} {} {}",'{', tree, '}'),
				')' => format_args!(
					|args: &::std::fmt::Arguments|{ format.write_fmt(args) },
					"( {} )", tree),
				']' => format_args!(
					|args: &::std::fmt::Arguments|{ format.write_fmt(args) },
					"[ {} ]", tree),
				_ => panic!()
			}
		}
				
	}
}

#[deriving(Show,Clone)]
pub enum NestingError {
	UnmatchedDelimiter(Token),
	UnclosedDelimiter(Token),
	Error(IoError)
}

fn nesting_rec<I: Iterator<IoResult<lexer::Token>>>(la: &mut LookAhead<IoResult<lexer::Token>, I>, arc: &Arc<String>) -> Result<Token, NestingError> {
	let start = match *la.peek() {
		Ok(ref x) => x.clone(),
		Err(_) => panic!()
	};
	let typ = match start.content {
		Other('{') => '}',
		Other('(') => ')',
		Other('[') => ']',
		_ => panic!()
	};
	let mut tree: Vec<Token> = Vec::new();
	while la.proceed() {
		let tok = match *la.peek() {
			Ok(ref x) => x.clone(),
			Err(ref e) => return Err(NestingError::Error(e.clone()))
		};
		match tok.content {
			Other('{') | Other('(') | Other('[') => match nesting_rec(la, arc) {
				Ok(t) => tree.push(t),
				e @ Err(_) => return e
			},
			Other(c @ '}') | Other(c @ ')') | Other(c @ ']') => if c == typ {
				return Ok(Token {
					content: Tree(typ, tree),
					ref_: CodeReference::from_lexer_token(&start, &tok, arc)
				})
			} else {
				return Err(NestingError::UnmatchedDelimiter(Token {
					ref_: CodeReference::from_lexer_token(&tok, &tok, arc),
					content: Tok(tok.content)
				}))
			},
			_ => tree.push(Token {
				ref_: CodeReference::from_lexer_token(&tok, &tok, arc),
				content: Tok(tok.content)
			})
		}
	}
	let ref_ = match tree.last() {
		None => CodeReference::from_lexer_token(&start, &start, arc),
		Some(x) => CodeReference::new(arc, CodePoint::new(start.line, start.start), x.ref_.end)
	};
	Err(NestingError::UnclosedDelimiter(Token {
		content: Tree(typ, tree),
		ref_: ref_
	}))
}

pub fn nesting<I: Iterator<IoResult<lexer::Token>>>(lex: I, file: String) -> Result<Vec<Token>, NestingError> {
	let mut la = match LookAhead::new(lex) {
		None => return Ok(Vec::new()),
		Some(x) => x
	};
	let arc = Arc::new(file);
	let mut tree: Vec<Token> = Vec::new();
	loop {
		let tok =  match la.peek() {
			&Ok(ref x) => x.clone(),
			&Err(ref e) => return Err(NestingError::Error(e.clone()))
		};
		match tok.content {
			Other('{') | Other('(') | Other('[') => match nesting_rec(&mut la, &arc) {
				Ok(t) => tree.push(t),
				Err(x) => return Err(x)
			},
			ref x @ Other('}') | ref x @ Other(')') | ref x @ Other(']') => return Err(NestingError::UnmatchedDelimiter(Token {
				ref_: CodeReference::from_lexer_token(&tok, &tok, &arc),
				content: Tok(x.clone())
			})),
			ref x @ _ => tree.push(Token {
				ref_: CodeReference::from_lexer_token(&tok, &tok, &arc),
				content: Tok(x.clone())
			})
		}
		if !la.proceed() {
			break;
		}
	}
	Ok(tree)
}