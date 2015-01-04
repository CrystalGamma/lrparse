use std::fmt::{Show, Formatter, Error};
use std::io::IoResult;
use nester::{Token, PrettyPrint};
use nester::TokenTree::*;
use {Rule, Node, RulePos, RuleItem, Grammar};

struct RefRulePos<'a>(&'a Rule, uint);

impl<'a> Show for RefRulePos<'a> {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		let &RefRulePos(rule, pos) = self;
		rule.fmt_internal(fmt, Some(pos))
	}
}

pub struct RefNode<'a, 'b>(&'a Grammar, &'b Node);

impl<'a, 'b> Show for RefNode<'a, 'b> {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		let &RefNode(grammar, node) = self;
		match fmt.write_str("{\n\t") {Err(_) => return Err(Error), _ => {}};
		let mut first = true;
		for state in node.state.iter() {
			if first {
				first = false;
			} else {
				match fmt.write_str(",\n\t") {Err(_) => return Err(Error), _ => {}};
			}
			try!(write!(fmt, "{}", state.with_grammar(grammar)));
		}
		match fmt.write_str("\n}") {Err(_) => return Err(Error), _ => {}};
		Ok(())
	}
}

/// describes an object that can be looked up in a grammar
pub trait WithGrammar<'a, 'b, T> {
	/// looks the object up in the dictionary
	fn with_grammar(&'b self, grammar: &'a Grammar) -> T;
}

impl<'a, 'b> WithGrammar<'a, 'b, RefRulePos<'a>> for RulePos {
	/// retrieves the information necessary information to print rule positions
	fn with_grammar(&'b self, grammar: &'a Grammar) -> RefRulePos<'a> {
		let &RulePos(rule, pos) = self;
		RefRulePos(&grammar.rules[rule], pos)
	}
}

impl<'a, 'b> WithGrammar<'a, 'b, RefNode<'a, 'b>> for Node {
	/// prepares to print a node's state
	fn with_grammar(&'b self, grammar: &'a Grammar) -> RefNode<'a, 'b> {
		RefNode(grammar, self)
	}
}

enum RefRuleItem<'a, 'b> {
	Symbol(&'b String, &'a Vec<Token>),
	Char(char)
}

impl<'a, 'b> RefRuleItem<'a, 'b> {
	/// writes the type of the runtime information of the parser for this symbol
	pub fn write_type<W: Writer>(&self, out: &mut W, indent: uint) -> IoResult<()> {
		match self {
			&RefRuleItem::Symbol(_, typ) => {
				try!(out.write_str("("));
				try!(typ.iter().pretty_print(out, indent, false));
				try!(out.write_str(")"));
			},
			_ => try!(out.write_str("()"))
		}
		Ok(())
	}
	/// writes the type definition as used in an enum
	pub fn write_type_definition<W: Writer>(&self, out: &mut W, indent: uint) -> IoResult<()> {
		use self::RefRuleItem::*;
		match self {
			&Symbol(name, typ) => {
				try!(out.write_str(name.as_slice()));
				if typ.len() != 0 {
					try!(self.write_type(out, indent));
				}
			},
			&Char(_) => try!(out.write_str("Other(char)"))
		}
		Ok(())
	}
	/// writes a pattern that matches the symbol
	pub fn write_pattern<W: Writer>(&self, out: &mut W, var: &str) -> IoResult<bool> {
		use self::RefRuleItem::*;
		match self {
			&Symbol(name, typ) => {
				try!(out.write_str("Token::"));
				try!(out.write_str(name.as_slice()));
				if typ.len() != 0 {
					try!(write!(out, "({})", var));
					return Ok(true);
				}
				Ok(false)
			},
			&Char(c) => {
				try!(out.write_str("Token::Other("));
				try!(Token::internal(Tok(::nester::Char(c))).pretty_print_token(out, 1));
				try!(out.write_str(")"));
				Ok(true)
			}
		}
	}
	/// writes the statement to pop a symbol from the parser stack
	pub fn write_pop_command<W: Writer>(&self,
					out: &mut W,
					capture_pos: Option<&str>,
					capture_val: uint)
					-> IoResult<()> {
		use self::RefRuleItem::Symbol;
		match (capture_pos, self) {
			(None, &Symbol(name, typ)) if typ.len() != 0 => {
				try!(write!(out, "
					let sym{}", capture_val));
				try!(out.write_str(" =  match self.stack.pop() { Some((_, Token::"));
				try!(out.write_str(name.as_slice()));
				out.write_str("(v), _)) => v, _ => panic!() };")
			},
			(Some(pos), &Symbol(name, typ)) if typ.len() != 0 => {
				try!(write!(out, "let (sym{}, {}", capture_val, pos));
				try!(out.write_str(") =  match self.stack.pop() { Some((_, Token::"));
				try!(out.write_str(name.as_slice()));
				out.write_str("(v), p)) => (v, p), _ => panic!() };")
			},
			(None, _) => out.write_str("
					self.stack.pop();"),
			(Some(pos), _) => {
				try!(out.write_str("
					let "));
				try!(out.write_str(pos));
				out.write_str(" = match self.stack.pop() { Some((_, _, p)) => p, _ => panic!() };")
			}
		}
	}
	/// checks if the symbol is a character symbol
	pub fn is_char(&self) -> bool {
		use self::RefRuleItem::*;
		match self {
			&Symbol(..) => false,
			&Char(_) => true
		}
	}
	/// checks if the symbol has no run-time information for the parser
	pub fn is_unit(&self) -> bool {
		use self::RefRuleItem::Symbol;
		match self {
			&Symbol(_, x) if x.len() != 0 => false,
			_ => true	// chars and unit symbols
		}
	}
}

impl<'a, 'b> WithGrammar<'a, 'b, RefRuleItem<'a, 'b>> for RuleItem {
	/// does the lookups to output a symbol
	fn with_grammar(&'b self, grammar: &'a Grammar) -> RefRuleItem<'a, 'b> {
		use self::RefRuleItem::*;
		use RuleItem::*;
		match self {
			&Chr(c) => Char(c),
			&Sym(ref sym) => match grammar.nterms.get(sym) {
				Some(ref x) => Symbol(sym, &x.type_),
				None => Symbol(sym, &grammar.terms[*sym])
			}
		}
	}
}