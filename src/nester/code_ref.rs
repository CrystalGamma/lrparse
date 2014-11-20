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

use std::sync::Arc;
use std::fmt::{Formatter, Show, FormatError, Arguments};

/// A human-readable offset into a text file
#[deriving(PartialEq,Clone)]
pub struct CodePoint {
	line: uint,
	column: uint
}

impl CodePoint {
	/// Constructs a new CodePoint from a line and column number
	pub fn new(line: uint, col: uint) -> CodePoint {
		CodePoint {
			line: line,
			column: col
		}
	}
}

/// A reference to range of characters in a text file
#[deriving(Clone)]
pub struct CodeReference {
	pub filename: Arc<String>,
	pub start: CodePoint,
	pub end: CodePoint
}

impl CodeReference {
	/// Constructs a new code reference from a file name and a pair of CodePoints
	pub fn new(file: &Arc<String>, start: CodePoint, end: CodePoint) -> CodeReference {
		CodeReference {
			filename: file.clone(),
			start: start,
			end: end
		}
	}

	/// Returns a reference to internal (not included in the source file) code
	pub fn internal() -> CodeReference {
		CodeReference::new(&Arc::new("$internal$".to_string()), CodePoint::new(0,0), CodePoint::new(0,0))
	}

	/// constructs a CodeReference from a rust-lexer token
	pub fn from_lexer_token(start: &lexer::Token, end: &lexer::Token, filename: &Arc<String>) -> CodeReference {
		CodeReference {
			filename: filename.clone(),
			start: CodePoint::new(start.line, start.start),
			end: CodePoint::new(end.line, end.end)
		}
	}
}

impl Show for CodeReference {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), FormatError> {
		if self.start.line == self.end.line {
			format_args!(|args: &Arguments| fmt.write_fmt(args),
				"{}:{}:{}-{}", self.filename.deref(), self.start.line, self.start.column, self.end.column)
		} else {
			format_args!(|args: &Arguments| fmt.write_fmt(args),
				"{}:{}:{}-{}:{}", self.filename.deref(), self.start.line, self.start.column, self.end.line, self.end.column)
		}
	}
}