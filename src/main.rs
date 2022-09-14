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

use std::path::PathBuf;

use boolegen::{filegen::LpLBooleGeneratorBuilder, lpl_syntax_tree::LplSyntaxTree};
use clap::Parser;

use anyhow::{bail, Context, Result};

#[derive(Debug, Parser)]
/// Generate a truth table for a given expression and outputs a file compatible with LPL Boole
struct Cli {
    /// The boolean expressions for which you want to generate a truth table
    #[clap(required = true, min_values = 1)]
    expressions: Vec<String>,

    /// Output filename
    #[clap(short, long)]
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

    /// Alias for -s 0
    #[clap(short, long, conflicts_with = "subexpressions")]
    all_subexpressions: bool,

    #[clap(short, long, value_name = "SECONDS", group = "time")]
    /// The seconds representing the time that LPL Boole is running since it was opened that will be written to the file.
    duration: Option<u64>,

    #[clap(
        short,
        long,
        number_of_values = 2,
        value_name = "BOUND",
        group = "time"
    )]
    /// Same as -d, but the seconds is a number chosen uniformely at random between the first and second provided number,
    /// where the first one is less than or equal to the second one
    random_duration: Vec<u64>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let mut trees = cli
        .expressions
        .into_iter()
        .map(|expr| {
            expr.parse::<LplSyntaxTree>()
                .with_context(|| format!("Invalid expression '{}'", expr))
        })
        .collect::<Result<Vec<_>, _>>()?;

    if cli.transform {
        trees
            .iter_mut()
            .for_each(LplSyntaxTree::transform_equivalent)
    }

    let mut generator = LpLBooleGeneratorBuilder::new();

    generator.write_subexpressions(if cli.all_subexpressions {
        Some(0)
    } else {
        cli.subexpressions
    });

    if let Some(duration) = cli.duration {
        generator.open_time(duration);
    } else if let &[min, max] = &cli.random_duration[..] {
        if min > max {
            bail!(
                "Lower bound for time cannot be greater than upper bound: '{}' > '{}'",
                min,
                max
            );
        }
        generator.random_open_time(min, max);
    }

    let output_name = cli.output.unwrap_or_else(|| "truth_table.tt".into());

    generator
        .build(&trees)
        .write_to_file(&output_name)
        .with_context(|| format!("Could not write contents to '{}'", output_name.display()))?;

    Ok(())
}
