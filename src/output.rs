use std::io::{File, Truncate, Write, IoResult, Writer};
use std::collections::HashMap;

use nester::{Token, Char, Tok, Tree, PrettyPrint};
use {Grammar, Node, RuleItem, Sym, Chr};

fn get_type<'a>(sym: &String, grammar: &'a Grammar) -> &'a Vec<Token> {
	match grammar.nterms.find(sym) {
		Some(ref x) => &x.type_,
		None => &grammar.terms[*sym]
	}
}

fn write_type<W: Writer>(out: &mut W, sym: &String, grammar: &Grammar, indent: uint) -> IoResult<bool> {
	let type_ = get_type(sym, grammar);
	if type_.len() != 0 {
		try!(actual_write_type(out, type_, indent));
		return Ok(true);
	}
	Ok(false)
}

fn actual_write_type<W: Writer>(out: &mut W, typ: &Vec<Token>, indent: uint) -> IoResult<()> {
	try!(out.write_str("("));
	try!(typ.iter().pretty_print(out, indent, false));
	out.write_str(")")
}

fn write_pattern<W: Writer>(out: &mut W, sym: &String, grammar: &Grammar, var: &str) -> IoResult<bool> {
	let typ = get_type(sym, grammar);
	actual_write_pattern(out, sym, typ, var)
}

fn actual_write_pattern<W: Writer>(out: &mut W, sym: &String, typ: &Vec<Token>, var: &str) -> IoResult<bool> {
	try!(out.write_str(sym.as_slice()));
	if typ.len() != 0 {
		try!(write!(out, "({})", var));
		return Ok(true);
	}
	Ok(false)
}

fn write_pop_command<W: Writer>(out: &mut W,
				capture_pos: Option<&str>,
				capture_val: Option<uint>,
				tok: &str) -> IoResult<()> {
	match (capture_pos, capture_val) {
		(None, None) => out.write_str("
				self.stack.pop();"),
		(Some(pos), None) => {
			try!(out.write_str("
				let "));
			try!(out.write_str(pos));
			out.write_str(" = match self.stack.pop() { Some((_, _, p)) => p, _ => panic!() };")
		},
		(None, Some(val)) => {
			try!(write!(out, "
				let sym{}", val));
			try!(out.write_str(" =  match self.stack.pop() { Some((_, "));
			try!(out.write_str(tok));
			out.write_str("(v), _)) => v, _ => panic!() };")
		},
		(Some(pos), Some(val)) => {
			try!(write!(out, "let (sym{}, {}", val, pos));
			try!(out.write_str(") =  match self.stack.pop() { Some((_, "));
			try!(out.write_str(tok));
			out.write_str("(v), p)) => (v, p), _ => panic!() };")
		}
	}
}

fn write_token_enum<W: Writer>(out: &mut W,
			mapping: &HashMap<RuleItem, uint>,
			grammar: &Grammar,
			debug: bool)
			-> IoResult<()> {
	if debug {
		try!(out.write_str("
#[deriving(Show,Clone)]"));
	}
	try!(out.write_str("
pub enum Token {
	Other(char)"));
	for (item, _) in mapping.iter() {
		if item == &Sym("$eof".to_string()) {
			continue;
		}
		match item {
			&Chr(_) => {},
			&Sym(ref s) => {
				println!("{}", s);
				try!(out.write_str(",\n\t"));
				try!(out.write_str(s.as_slice()));
				try!(write_type(out, s, grammar, 1));
			}
		}
	}
	out.write_str("
}")
}

fn write_error_enum<W: Writer>(out: &mut W, errors: &HashMap<String, Vec<Token>>, debug: bool) -> IoResult<()> {
	if debug {
		try!(out.write_str("
#[deriving(Show,Clone)]"));
	}
	try!(out.write_str("
pub enum Error {
	SyntaxError(CodeReference)"));
	for (name, typ) in errors.iter() {
		try!(out.write_str(",
	"));
		try!(out.write_str(name.as_slice()));
		if typ.len() != 0 {
			try!(actual_write_type(out, typ, 1));
		}
	}
	out.write_str("
}")
}

pub fn write_parser(filename: &Path,
		nodes: Vec<Node>,
		mapping: HashMap<RuleItem, uint>,
		num_symbols: uint,
		grammar: &Grammar,
		debug: bool)
		-> IoResult<()> {
	let mut file = try!(File::open_mode(filename, Truncate, Write));
	let out = &mut file;
	try!(grammar.prelude.iter().pretty_print(out, 0, false));
	try!(write_token_enum(out, &mapping, grammar, debug));
	try!(write_error_enum(out, &grammar.errors, debug));
	try!(out.write_str("
static TABLE: &'static [uint] = &["));
	let num_rules = grammar.rules.len();
	for node in nodes.into_iter() {
		let mut line: Vec<uint> = Vec::with_capacity(num_symbols);
		line.grow(num_symbols, match node.reduce {
			Some(x) => x + 1,
			None => 0u
		});
		for (shift, target) in node.shifts.into_iter() {
			let tok_id = mapping[shift];
			match shift {
				Chr(_) => { line[tok_id] = target + num_rules + 1; continue; },
				Sym(ref s) => match grammar.nterms.find(s) {
					None => { line[tok_id] = target + num_rules + 1; }
					Some(_) => { line[tok_id] = target; }
				}
			}
			
		}
		println!("{}", line);
		try!(out.write_str("\n"));
		for num in line.into_iter() {
			try!(write!(out, "{:8u},", num));
		}
	}
	try!(write!(out, "];
static NUM_RULES: uint = {}u;\nstatic NUM_SYMBOLS: uint = {}u;\n",
			grammar.rules.len(), num_symbols));
	if debug {
		try!(out.write_str("
#[deriving(Show)]"));
	}
	try!(out.write_str("
pub struct Parser {
	stack: Vec<(uint, Token, CodeReference)>,
}

impl Parser {
	pub fn new(pos: CodeReference) -> Parser {
		Parser {stack: vec![(0u, Other('#'), pos)]}
	}
	fn get_token_id(tok: &Token) -> uint {
		match tok {"));
	for (item, &id) in mapping.iter() {
		match item {
			&Chr(c @ _) => {
				try!(out.write_str("\n\t\t\t&Other"));
				try!(Token::internal(Tree(')',
					vec![Token::internal(Tok(Char(c)))])).pretty_print_token(out, 1));
				try!(write!(out, " => {},", id));
			},
			&Sym(ref s) if id != 0 => {
				try!(out.write_str("\n\t\t\t&"));
				try!(write_pattern(out, s, grammar, "_"));
				try!(write!(out,"=> {},", id));
			}
			_ => {}
		}
	}
	try!(out.write_str("\n\t\t\t_ => panic!(\"unknown token used in parser\")
		}
	}
	pub fn consume_token(&mut self, tok: Token, pos: CodeReference) -> Result<(), Error> {
		let tok_id = Parser::get_token_id(&tok);
		try!(self.do_reduces(tok_id, &pos));
		let &(state, _, _) = match self.stack.last() {Some(x) => x, None => panic!()};
		self.stack.push((TABLE[state*NUM_SYMBOLS + tok_id]-NUM_RULES-1, tok, pos));
		Ok(())
	}
	fn do_reduces(&mut self, tok_id: uint, redpos: &CodeReference) -> Result<(), Error> {
		loop {
			let &(state, _, _) = match self.stack.last() {Some(x) => x, None => panic!()};
			let action = TABLE[state*NUM_SYMBOLS + tok_id];
			if action > NUM_RULES {
				return Ok(());
			}
			try!(self.reduce(action, redpos.get_start()));
		}
	}
	fn reduce(&mut self, rule: uint, redpos: CodeReference) -> Result<(), Error> {
		match match rule {
			0 => Err(SyntaxError(redpos)),
			1 => {
				"));
	let start_sym = match grammar.rules[0].seq[0] {
		Sym(ref x) => x,
		_ => panic!()
	};
	let typ = get_type(start_sym, grammar);
	if typ.len() != 0 {
		try!(out.write_str("let (sym, pos) = match self.stack.pop() { Some((_, "));
		try!(actual_write_pattern(out, start_sym, typ, "x"));
		try!(out.write_str(", p)) => (x, p), _ => panic!() };
				self.stack.push((1, Accept_(sym), pos));"));
	} else {
		try!(out.write_str("let pos = match self.stack.pop() {
					Some((_, _, p)) => p,
					_ => panic!()
				};
				self.stack.push((1, Accept_, pos));"));
	}
	try!(out.write_str("
				return Ok(());
			},"));
	let mut rule_id = 0u;
	for rule in grammar.rules.iter() {
		if rule_id == 0u {
			rule_id += 1;
			continue;
		}
		if !mapping.contains_key(&Sym(rule.nterm.deref().clone())) {
			rule_id += 1;
			continue;	// prevent crashes if nonterminal is not used / has no mapping
		}
		try!(write!(out, "\n\t\t\t{} => {}", rule_id + 1, '{'));
		let len = rule.seq.len();
		for i in range(0, len) {
			let cap_pos = if i == len - 1 {
				Some("startpos")
			} else if i == 0 {
				Some("endpos")
			} else {
				None
			};
			match rule.seq[len - i -1] {
				Chr(_) => try!(write_pop_command(out, cap_pos, None, "Other")),
				Sym(ref s) => {
					let typ = get_type(s, grammar);
					try!(write_pop_command(out, cap_pos, if typ.len() == 0 {
						None
					} else {
						Some(len - i - 1)
					}, s.as_slice()));
				}
			}
		}
		let is_unit = grammar.nterms[*rule.nterm.deref()].type_.len() == 0;
		try!(out.write_str("
				"));
		if len > 1 {
			try!(out.write_str("let pos = startpos.range(&endpos);"));
		} else if len == 1 {
			try!(out.write_str("let pos = startpos;"));
		} else {
			try!(out.write_str("let pos = redpos;"));
		}
		try!(out.write_str("
				"));
		if !is_unit {
			try!(out.write_str("let res = "));
		}
		try!(write!(out, "try!(self.rule{}((", rule_id));
		for i in range(0, len).filter(|&i: &uint| get_type(match rule.seq[i] {
			Chr(_) => return false,
			Sym(ref s) => s
		}, grammar).len() != 0) {
			try!(write!(out, "sym{}, ", i));
		}
		try!(out.write_str("), pos));
				"));
		if is_unit {
			try!(write!(out, "Ok(({}, {}, pos))", mapping[Sym(rule.nterm.deref().clone())], rule.nterm));
		} else {
			try!(write!(out, "Ok(({}, {}(res), pos))", mapping[Sym(rule.nterm.deref().clone())], rule.nterm));
		}
		try!(out.write_str("
			}"));
		rule_id += 1;
	}
	try!(out.write_str("
			_ => panic!()
		} {
			Ok((id, x, pos)) => {
				let &(st, _, _) = match self.stack.last() {Some(x) => x, None => panic!()};
				let goto = TABLE[st * NUM_SYMBOLS + id];
				self.stack.push((goto, x, pos));
				Ok(())
			},
			Err(e) => Err(e)
		}
	}"));
	for rule_id in range(1, grammar.rules.len()) {	// FIXME: can't borrow grammar here
		{
		let rule = &grammar.rules[rule_id];
		if !mapping.contains_key(&Sym(rule.nterm.deref().clone())) {
			continue;	// prevent crashes if nonterminal is not used / has no mapping
		}
		try!(write!(out, "\n\tfn rule{}(&mut self, symbols: (", rule_id));
		let len = rule.seq.len();
		for i in range(0u, len) {
			match rule.seq[i] {
				Chr(_) => {},
				Sym(ref s) => {
					if try!(write_type(out, s, grammar, 1)) {
						try!(out.write_str(", "));
					}
				}
			}
		}
		}
		try!(out.write_str("), pos: CodeReference) -> Result<"));
		if !try!(write_type(out, grammar.rules[rule_id].nterm.deref(), grammar, 1)) {
			try!(out.write_str("()"));
		}
		try!(out.write_str(", Error> "));
		try!(grammar.rules[rule_id].code.pretty_print_token(out, 1));
	}
	try!(out.write_str("
	pub fn end_parse(mut self, pos: CodeReference) -> Result<"));
	if !try!(write_type(out, &"Accept_".to_string(), grammar, 1)) {
		try!(out.write_str("()"));
	}
	try!(out.write_str(", Error> {
		loop {
			let state = match self.stack.last() {
				Some(&(_, "));
	if try!(write_pattern(out, &"Accept_".to_string(), grammar, "x")) {
		try!(out.write_str(", _)) => return Ok(x),"));
	} else {
		try!(out.write_str(", _)) => return Ok(()),"));
	}
	try!(out.write_str("
				Some(&(x, _, _)) => x,
				None => panic!()
			};
			let action = TABLE[state*NUM_SYMBOLS];
			try!(self.reduce(action, pos.get_start()));
		}
	}
}"));
	Ok(())
}