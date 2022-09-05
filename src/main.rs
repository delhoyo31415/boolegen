use std::{fs, path::PathBuf};

use boolegen::{filegen::LplBooleGenerator, parser::SyntaxTree};
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
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let mut tree: SyntaxTree = cli.expression.parse().context("Invalid expression")?;
    if cli.transform {
        tree.transform_equivalent();
    }

    let lpl_output = LplBooleGenerator::new(&tree)
        .context("Invalid expression for generator")?
        .into_string();

    let output_name = cli.output.unwrap_or_else(|| "truth_table.tt".into());
    fs::write(&output_name, lpl_output)
        .with_context(|| format!("Could not write contents to '{}'", output_name.display()))?;

    Ok(())
}
