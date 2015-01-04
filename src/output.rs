use std::io::{File, Truncate, Write, IoResult, Writer};
use std::collections::HashMap;

use nester::{Token, PrettyPrint};
use {Grammar, Node, RuleItem, Rule};
use show::WithGrammar;
use RuleItem::*;

/// writes the definition of the symbol enumeration
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
		let itm = item.with_grammar(grammar);
		if !itm.is_char() {
			try!(out.write_str(",
	"));
			try!(itm.write_type_definition(out, 1));
		}
	}
	out.write_str("
}")
}

/// writes the definition of the error code enumeration
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
			try!(out.write_str("("));
			try!(typ.iter().pretty_print(out, 1, false));
			try!(out.write_str(")"));
		}
	}
	out.write_str("
}")
}

/// writes the state transition / action table
fn write_table<W: Writer, I: Iterator<Node>>(out: &mut W,
					mut nodes: I,
					grammar: &Grammar,
					num_symbols: uint,
					mapping: &HashMap<RuleItem, uint>)
					-> IoResult<()> {
	try!(out.write_str("
static TABLE: &'static [uint] = &["));
	let num_rules = grammar.rules.len();
	for node in nodes {
		let mut line: Vec<uint> = Vec::with_capacity(num_symbols);
		line.grow(num_symbols, match node.reduce {
			Some(x) => x + 1,
			None => 0u
		});
		for (shift, target) in node.shifts.into_iter() {
			let tok_id = mapping[shift];
			match shift {
				Chr(_) => { line[tok_id] = target + num_rules + 1; continue; },
				Sym(ref s) => match grammar.nterms.get(s) {
					None => { line[tok_id] = target + num_rules + 1; }
					Some(_) => { line[tok_id] = target; }
				}
			}
			
		}
		println!("{}", line);
		try!(out.write_str("\n"));
		for num in line.into_iter() {
			try!(write!(out, "{:8},", num));
		}
	}
	out.write_str("];")
}

/// writes the function containing the code associated to a rule
fn write_rule_fn<W: Writer>(out: &mut W, rule_id: uint, rule: &Rule, grammar: &Grammar) -> IoResult<()> {
		try!(write!(out, "\n\tfn rule{}(&mut self, symbols: (", rule_id));
		let len = rule.seq.len();
		for i in range(0u, len) {
			let sym = rule.seq[i].with_grammar(grammar);
			if !sym.is_unit() {
				try!(sym.write_type(out, 1));
				try!(out.write_str(", "));
			}
		}
		try!(out.write_str("), pos: &CodeReference) -> Result<"));
		let sym = Sym(grammar.rules[rule_id].nterm.deref().clone());
		try!(sym.with_grammar(grammar).write_type(out, 1));
		try!(out.write_str(", Error> "));
		grammar.rules[rule_id].code.pretty_print_token(out, 1)
}

/// writes the parser to a file
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
	try!(write_table(out, nodes.into_iter(), grammar, num_symbols, &mapping));
	try!(write!(out, "
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
		Parser {stack: vec![(0u, Token::Other('#'), pos)]}
	}
	fn get_token_id(tok: &Token) -> uint {
		match tok {"));
	for (item, id) in mapping.iter() {
		if item.is_eof() {
			continue;
		}
		let itm = item.with_grammar(grammar);
		try!(out.write_str("\n\t\t\t&"));
		try!(itm.write_pattern(out, "_"));
		try!(write!(out, " => {}u,", id));
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
			0 => Err(Error::SyntaxError(redpos)),
			1 => {
				"));
	let start_sym = grammar.rules[0].seq[0].with_grammar(grammar);
	if !start_sym.is_unit() {
		try!(out.write_str("let (sym, pos) = match self.stack.pop() { Some((_, "));
		try!(start_sym.write_pattern(out, "x"));
		try!(out.write_str(", p)) => (x, p), _ => panic!() };
				self.stack.push((1, Accept_(sym), pos));"));
	} else {
		try!(out.write_str("let pos = match self.stack.pop() {
					Some((_, _, p)) => p,
					_ => panic!()
				};
				self.stack.push((1, Token::Accept_, pos));"));
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
			try!(rule.seq[len - i -1].with_grammar(grammar).write_pop_command(out, cap_pos, len - i- 1));
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
		for i in range(0, len).filter(|&i: &uint| !rule.seq[i].with_grammar(grammar).is_unit()) {
			try!(write!(out, "sym{}, ", i));
		}
		try!(out.write_str("), &pos));
				"));
		if is_unit {
			try!(write!(out, "Ok(({}, Token::{}, pos))", mapping[Sym(rule.nterm.deref().clone())], rule.nterm));
		} else {
			try!(write!(out, "Ok(({}, Token::{}(res), pos))", mapping[Sym(rule.nterm.deref().clone())], rule.nterm));
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
	let mut rule_id = 0;
	for rule in grammar.rules.iter() {
		if rule_id > 0 && mapping.contains_key(&Sym(rule.nterm.deref().clone())) {
			try!(write_rule_fn(out, rule_id, rule, grammar));
		}
		rule_id += 1;
	}
	try!(out.write_str("
	pub fn end_parse(mut self, pos: CodeReference) -> Result<"));
	let accept = Sym("Accept_".to_string());
	let acc = accept.with_grammar(grammar);
	try!(acc.write_type(out, 1));
	try!(out.write_str(", Error> {
		loop {
			let state = match self.stack.last() {
				Some(&(_, "));
	try!(acc.write_pattern(out, "x"));
	if !acc.is_unit() {
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