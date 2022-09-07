// Copyright (C) 2022 Pablo del Hoyo Abad <pablodelhoyo1314@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

use std::{fs, path::PathBuf};

use boolegen::{filegen::LpLBooleGeneratorBuilder, parser::SyntaxTree};
use clap::Parser;

use anyhow::{Context, Result};

#[derive(Debug, Parser)]
/// Generate a truth table for a given expression and outputs a file compatible with LPL Boole
struct Cli {
    /// The boolean expression for which you want to generate a truth table    
    expression: String,
    /// Output filename
    output: Option<PathBuf>,
    /// Modify the AST of the expression to get an equivalent one using the associative property of AND and OR binary operators,
    /// resulting in a expression with less parenthesis
    #[clap(short, long)]
    transform: bool,
    /// Show subexpressions in different columns with a degree of at least <MIN_DEGREE>
    ///
    /// In case the given number is greater than the degree of the main expression then
    /// only the main expression will be written
    #[clap(short, long, value_name = "MIN_DEGREE")]
    subexpressions: Option<u32>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let mut tree: SyntaxTree = cli.expression.parse().context("Invalid expression")?;
    if cli.transform {
        tree.transform_equivalent();
    }

    let lpl_output = LpLBooleGeneratorBuilder::new()
        .write_subexpressions(cli.subexpressions)
        .build(&tree)
        .context("Invalid expression for LPL File")?
        .into_string();

    let output_name = cli.output.unwrap_or_else(|| "truth_table.tt".into());
    fs::write(&output_name, lpl_output)
        .with_context(|| format!("Could not write contents to '{}'", output_name.display()))?;

    Ok(())
}
