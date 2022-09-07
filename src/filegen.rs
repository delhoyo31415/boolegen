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

use std::time::SystemTime;

use crate::{
    boolean_value::{BooleanValue, BooleanVariations, ColumnsBooleanVariations},
    error::FileGeneratorError,
    parser::{BinaryOperator, Env, ExpressionNode, SyntaxTree},
};

pub struct LpLBooleGeneratorBuilder {
    capacity: usize,
    write_subexpressions: Option<u32>,
}

impl LpLBooleGeneratorBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn write_subexpressions(&mut self, minimum_degree: Option<u32>) -> &mut Self {
        self.write_subexpressions = minimum_degree;
        self
    }

    pub fn with_capacity(&mut self, capacity: usize) -> &mut Self {
        self.capacity = capacity;
        self
    }

    pub fn build<'a>(
        &self,
        tree: &'a SyntaxTree,
    ) -> Result<LplBooleGenerator<'a>, FileGeneratorError> {
        LplBooleGenerator::from_builder(self, tree)
    }
}

impl Default for LpLBooleGeneratorBuilder {
    fn default() -> Self {
        // A default capacity of 4KB
        Self {
            capacity: 4 * 1024,
            write_subexpressions: None,
        }
    }
}

pub struct LplBooleGenerator<'a> {
    syntax_tree: &'a SyntaxTree,
    write_subexpressions: Option<u32>,
    output: String,
}

impl<'a> LplBooleGenerator<'a> {
    fn from_builder(
        builder: &LpLBooleGeneratorBuilder,
        syntax_tree: &'a SyntaxTree,
    ) -> Result<Self, FileGeneratorError> {
        if syntax_tree.env().can_be_lpl_encoded() {
            Ok(Self {
                syntax_tree,
                output: String::with_capacity(builder.capacity),
                write_subexpressions: builder.write_subexpressions,
            })
        } else {
            Err(FileGeneratorError::InvalidExpression(
                "Variables must start with a capital letter".to_string(),
            ))
        }
    }

    pub fn new(syntax_tree: &'a SyntaxTree) -> Result<Self, FileGeneratorError> {
        LpLBooleGeneratorBuilder::new().build(&syntax_tree)
    }

    fn write_headers(&mut self) {
        self.output += "4.0.0.22673\rwnds:Windows 76.1\rBleF\rC";

        // Timestamp when LPL Boole was opened
        let open_timestamp = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .expect("Time went before EPOCH")
            .as_millis();

        // Timestamp when the file is saved
        // TODO: this should be randomized or chosen by the user
        let close_timestamp = open_timestamp + 3 * 60 + 17;

        self.output += open_timestamp.to_string().as_str();
        self.output += "D";
        self.output += close_timestamp.to_string().as_str();
        self.output += "\rnewFormat\r"
    }

    fn write_generated_column(&mut self, values: &[BooleanValue], name: &str) {
        self.output += "openproof.boole.BooleExpressionData=openproof.boole.BooleExpressionData{";
        self.output += "_fLabelNum=0;_fLabelText=\"\";_fByBoole=true;";
        self.output +=
            "_fTruthColumnExist:1[openproof.boole.TruthColumnData=openproof.boole.TruthColumnData{";
        self.output += "v=\"";
        self.output
            .extend(values.iter().map(BooleanValue::lpl_boole_encoded));
        self.output += "\\000\";_fCharIndex=0;_fByBoole=true;";
        self.output += "}"; // TruthColumnData
        self.output += "]_fExpression="; // TruthColumnExist
        self.output += name;
        self.output += ";_fStatusColumn@o()";
        self.output += "}"; // BooleExpressionData
    }

    fn write_generated_columns(&mut self) {
        let count = self.syntax_tree.env().var_count();
        let mut bool_iter = ColumnsBooleanVariations::reversed(count);

        for (idx, name) in self.syntax_tree.env().names_iter().enumerate() {
            let values = bool_iter.next_variation().unwrap();

            // TODO: ensure the name of the variable is allowed by LPL Boole
            self.write_generated_column(values, name);

            if idx != count - 1 {
                self.output += ",";
            }
        }
    }

    fn write_checksums(&mut self) {
        let checksum = simple_checksum(&self.output).to_string();
        self.output += "c=";
        self.output += &checksum;
        self.output += ";\r";

        let checksum = circle_shift_checksum(&self.output).to_string();
        self.output += "s=";
        self.output += &checksum;
        self.output += ";";
    }

    fn write_subcolumns(&mut self, subcols_data: &[SubColumnData]) {
        self.output += "_fTruthColumnExist:";
        self.output += subcols_data.len().to_string().as_str();
        self.output += "[";
        for (idx, data) in subcols_data.iter().enumerate() {
            data.write(&mut self.output);
            if idx != subcols_data.len() - 1 {
                self.output += ",";
            }
        }
        self.output += "]";
    }

    fn write_answer_column(&mut self, expr: &str, subcols_data: &[SubColumnData]) {
        self.output += "openproof.boole.BooleExpressionData=openproof.boole.BooleExpressionData{";
        self.output += "_fLabelNum=0;_fLabelText=\"\";_fByBoole=false;";
        self.write_subcolumns(subcols_data);
        self.output += "_fExpression=\"";
        self.output += expr;
        self.output += "\";";
        self.write_status_column(1 << self.syntax_tree.env().var_count());
        self.output += "}";
    }

    fn write_answer_columns(&mut self) {
        // The env is the same for the subexpressions
        let env = self.syntax_tree.env();

        if let Some(min_degree) = self.write_subexpressions {
            let min_degree = self.syntax_tree.degree().min(min_degree as usize);

            for node in self.syntax_tree.postorder_traversal() {
                if node.is_var() || node.degree() < min_degree {
                    continue;
                }

                let cols = generate_subcolumn_data(node, env);
                self.write_answer_column(node.lpl_formatted().as_str(), &cols);
                self.output += ",";
            }
            // This won't remove a character which is not a ',' because it is guarenteed that the last
            // character is that one
            self.output.pop();
        } else {
            let cols = generate_subcolumn_data(self.syntax_tree.root(), env);
            self.write_answer_column(self.syntax_tree.lpl_formatted().as_str(), &cols);
        }
    }

    fn write_truth_table_row_status(&mut self, is_corrected: bool, num: usize) {
        let c_value = u32::from(is_corrected).to_string();

        self.output += "openproof.boole.status.TruthTableRowStatus=openproof.boole.status.TruthTableRowStatus{";
        self.output += "c=";
        self.output += &c_value;
        self.output += ";s=\"\";l=\"\";d@k=\"\";t=false;r=";
        self.output += num.to_string().as_str();
        self.output += ";}";
    }

    fn write_status_column(&mut self, num_columns: usize) {
        self.output += "_fStatusColumn(";
        for num in 0..num_columns {
            self.write_truth_table_row_status(true, num);
            self.output += ",";
        }
        self.write_truth_table_row_status(false, num_columns);
        self.output += ")o()";
    }

    fn write_all(&mut self) {
        self.write_headers();

        self.output += "=openproof.zen.Openproof{";
        self.output += "p=openproof.boole.Boole{";
        self.output += "_fAssessmentData=openproof.boole.entities.AssessmentData{";
        self.output += "_fTitle=@;_fRefData=openproof.boole.entities.ExpressionPanelData{";
        self.output += "_fExpVector(";
        self.write_generated_columns();
        self.output += ")_fIsReferenceSide=true;"; // fExpVector;
        self.output += "}"; // RefData=ExpressionPanelData
        self.output += "_fSentData=openproof.boole.entities.ExpressionPanelData{";
        self.output += "_fExpVector(";
        self.write_answer_columns();
        self.output += ")_fIsReferenceSide=false;"; // fExpVector
        self.output += "}"; // SentData=ExpressionPanelData
        self.output += "_fIsTaut=@;_fIsTTPossible=@;_fAreTautEquiv=@;_fIsLastSentenceTautCon=@;";
        self.output += "_fIsFirstSentenceTautCon=@;_fNeedToBeComplete=@;isContra=@;isTTContra=@;";
        self.output += "}"; // _fAssessmentData=openproof.boole.entities.AssessmentData{
        self.output += "}"; // p=openproof.boole.Boole{
        self.output += "}"; //=openproof.zen.Openproof

        self.write_checksums();
    }

    pub fn into_string(mut self) -> String {
        self.write_all();
        self.output
    }
}

fn generate_subcolumn_data(node: &ExpressionNode, env: &Env) -> Vec<SubColumnData> {
    let var_count = env.var_count();
    let mut data = vec![SubColumnData::with_capacity(1 << var_count); node.degree()];

    let mut iter = BooleanVariations::reversed(var_count);
    let mut stored_values = LplIntermediateEval::new(node, env);
    while let Some(inputs) = iter.next_variation() {
        stored_values.eval(inputs);
        for (col, &value) in data.iter_mut().zip(stored_values.intermediates()) {
            col.add(value)
        }
    }

    data
}

pub fn simple_checksum<I: AsRef<[u8]>>(input: I) -> u64 {
    input
        .as_ref()
        .iter()
        .filter(|&&byte| byte != 0x0D && byte != 0x0A)
        .map(|&byte| byte as u64)
        .sum()
}

pub fn circle_shift_checksum<I: AsRef<[u8]>>(input: I) -> u64 {
    let input = input.as_ref();

    let mut checksum = 0;
    let mut shifter = 0;

    for &byte in input {
        let byte = byte as u64;

        if byte == 0x0D || byte == 0x0A {
            continue;
        }
        checksum += (byte << shifter ^ byte >> 7 & 0x1 ^ 0xFFFFFFFF) & 0xFF;
        checksum &= 0xFFFFFFF;
        shifter = (shifter + 1) % 8;
    }

    checksum
}

impl Env {
    fn can_be_lpl_encoded(&self) -> bool {
        // LPL boole only accepts as a variable a string which is
        // alphabetic and whose first letter start is capitalized is capitalized
        self.names_iter().all(|name| {
            let mut chars = name.chars();
            if let Some(first_letter) = chars.next() {
                first_letter.is_ascii_uppercase() && chars.all(char::is_alphabetic)
            } else {
                false
            }
        })
    }
}

// Evaluates the expression, storing the intermediate results
// in inorder and ignoring the variables
struct LplIntermediateEval<'a> {
    node: &'a ExpressionNode,
    env: &'a Env,
    results: Vec<BooleanValue>,
}

impl<'a> LplIntermediateEval<'a> {
    pub fn new(node: &'a ExpressionNode, env: &'a Env) -> Self {
        let values = node.degree();
        Self {
            node,
            env,
            results: vec![BooleanValue::False; values],
        }
    }

    pub fn eval(&mut self, inputs: &[BooleanValue]) -> BooleanValue {
        let (final_result, _) = self.intermediate_eval(self.node, inputs, 0);
        final_result
    }

    pub fn intermediates(&self) -> &[BooleanValue] {
        &self.results
    }

    fn intermediate_eval(
        &mut self,
        node: &ExpressionNode,
        inputs: &[BooleanValue],
        store_idx: usize,
    ) -> (BooleanValue, usize) {
        match node {
            ExpressionNode::BinaryExpression(lhs, rhs, op) => {
                let (lhs_value, store_idx) = self.intermediate_eval(lhs, inputs, store_idx);
                let (rhs_value, next_store_idx) =
                    self.intermediate_eval(rhs, inputs, store_idx + 1);
                let result = op.apply(lhs_value, rhs_value);
                self.results[store_idx] = result;
                (result, next_store_idx)
            }
            ExpressionNode::NotExpression(expr) => {
                let (value, next_store_idx) = self.intermediate_eval(expr, inputs, store_idx + 1);
                let result = value.not();
                self.results[store_idx] = result;
                (result, next_store_idx)
            }
            ExpressionNode::Var(_) => (node.eval(self.env, inputs), store_idx),
        }
    }
}

impl ExpressionNode {
    // TODO: think on a way which does not involve a heap allocations per stack frame
    fn lpl_formatted(&self) -> String {
        match self {
            ExpressionNode::BinaryExpression(lhs, rhs, op) => {
                let lhs = if let ExpressionNode::BinaryExpression(.., lhs_op) = &**lhs {
                    if op == lhs_op && op.is_associative() {
                        lhs.lpl_formatted()
                    } else {
                        format!("({})", lhs.lpl_formatted())
                    }
                } else {
                    lhs.lpl_formatted()
                };

                // RHS is treated differently than LHS to ensure LPL Boole generates the same
                // AST as this program does
                let rhs = if let ExpressionNode::BinaryExpression(..) = &**rhs {
                    format!("({})", rhs.lpl_formatted())
                } else {
                    rhs.lpl_formatted()
                };

                format!("{} {} {}", lhs, op.lpl_boole_encoded(), rhs)
            }
            ExpressionNode::NotExpression(not_expr) => {
                let parenthesized = not_expr.lpl_formatted();
                if not_expr.is_var() {
                    format!("~{parenthesized}")
                } else {
                    format!("~({parenthesized})")
                }
            }
            ExpressionNode::Var(name) => name.to_string(),
        }
    }
}

impl SyntaxTree {
    fn lpl_formatted(&self) -> String {
        self.root().lpl_formatted()
    }
}

impl BinaryOperator {
    fn lpl_boole_encoded(&self) -> &'static str {
        match self {
            BinaryOperator::And => "&",
            BinaryOperator::Or => "|",
            BinaryOperator::Conditional => "$",
            BinaryOperator::Biconditional => "%",
        }
    }
}

#[derive(Debug, Clone)]
struct SubColumnData {
    values: Vec<BooleanValue>,
}

impl SubColumnData {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            values: Vec::with_capacity(capacity),
        }
    }

    pub fn add(&mut self, value: BooleanValue) {
        self.values.push(value);
    }

    pub fn write(&self, output: &mut String) {
        *output += "openproof.boole.TruthColumnData=openproof.boole.TruthColumnData{";
        *output += "v=\"";
        output.extend(self.values.iter().map(BooleanValue::lpl_boole_encoded));
        *output += "\\000\";_fCharIndex=0;_fByBoole=false;";
        *output += "}";
    }
}

impl BooleanValue {
    fn lpl_boole_encoded(&self) -> &'static str {
        match self {
            BooleanValue::True => "T",
            BooleanValue::False => "F",
        }
    }
}
