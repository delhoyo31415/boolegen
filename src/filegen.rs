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

use std::{fs::File, io::Write, path::Path, time::SystemTime};

use crate::{
    boolean_value::{BooleanValue, BooleanVariations, ColumnsBooleanVariations},
    error::BoolegenError,
    lpl_syntax_tree::LplSyntaxTree,
    parser::{BinaryOperator, Env, ExpressionNode},
};

pub struct LpLBooleGeneratorBuilder {
    buffer_capacity: usize,
    // If it is not None, write the subexpressions with at least a given degree
    write_subexpressions: Option<u32>,
    // Time spent since the file was open and saved, in seconds
    seconds_spent: u32,
}

impl LpLBooleGeneratorBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn write_subexpressions(&mut self, minimum_degree: Option<u32>) -> &mut Self {
        self.write_subexpressions = minimum_degree;
        self
    }

    pub fn random_open_time(&mut self, min: u32, max: u32) -> &mut Self {
        use rand::Rng;
        if max < min {
            panic!("lower bound cannot be higher than upper bound");
        }
        self.seconds_spent = rand::thread_rng().gen_range(min..=max);
        self
    }

    pub fn open_time(&mut self, seconds_spent: u32) -> &mut Self {
        self.seconds_spent = seconds_spent;
        self
    }

    pub fn with_capacity(&mut self, buffer_capacity: usize) -> &mut Self {
        self.buffer_capacity = buffer_capacity;
        self
    }

    pub fn build<'a>(&self, trees: &'a [LplSyntaxTree]) -> LplBooleGenerator<'a> {
        LplBooleGenerator::from_builder(self, trees)
    }
}

impl Default for LpLBooleGeneratorBuilder {
    fn default() -> Self {
        // A default capacity of 4KB
        Self {
            buffer_capacity: 4 * 1024,
            write_subexpressions: None,
            seconds_spent: 3 * 60 + 17,
        }
    }
}

pub struct LplBooleGenerator<'a> {
    trees: &'a [LplSyntaxTree],
    buffer_capacity: usize,
    write_subexpressions: Option<u32>,
    time_spent: u32,
    env: Env,
}

impl<'a> LplBooleGenerator<'a> {
    fn from_builder(builder: &LpLBooleGeneratorBuilder, trees: &'a [LplSyntaxTree]) -> Self {
        let env = Env::merged(trees.iter().map(LplSyntaxTree::env));

        Self {
            trees,
            env,
            buffer_capacity: builder.buffer_capacity,
            write_subexpressions: builder.write_subexpressions,
            time_spent: builder.seconds_spent,
        }
    }

    pub fn new(trees: &'a [LplSyntaxTree]) -> Self {
        LpLBooleGeneratorBuilder::new().build(trees)
    }

    fn write_headers(&self, output: &mut String) {
        output.push_str("4.0.0.22673\rwnds:Windows 76.1\rBleF\rC");

        // Timestamp when LPL Boole was opened
        let open_timestamp = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .expect("Time went before EPOCH")
            .as_millis();

        // Timestamp when the file is saved
        let close_timestamp = open_timestamp + self.time_spent as u128;

        output.push_str(open_timestamp.to_string().as_str());
        output.push('D');
        output.push_str(close_timestamp.to_string().as_str());
        output.push_str("\rnewFormat\r");
    }

    fn write_generated_column(&self, output: &mut String, values: &[BooleanValue], name: &str) {
        output.push_str("openproof.boole.BooleExpressionData=openproof.boole.BooleExpressionData{");
        output.push_str("_fLabelNum=0;_fLabelText=\"\";_fByBoole=true;");
        output.push_str(
            "_fTruthColumnExist:1[openproof.boole.TruthColumnData=openproof.boole.TruthColumnData{",
        );
        output.push_str("v=\"");
        output.extend(values.iter().map(BooleanValue::lpl_boole_encoded));
        output.push_str("\\000\";_fCharIndex=0;_fByBoole=true;");
        output.push('}'); // TruthColumnData
        output.push_str("]_fExpression="); // TruthColumnExist
        output.push_str(name);
        output.push_str(";_fStatusColumn@o()");
        output.push('}'); // BooleExpressionData
    }

    fn write_generated_columns(&self, output: &mut String) {
        let count = self.env.var_count();
        let mut bool_iter = ColumnsBooleanVariations::reversed(count);

        for (idx, name) in self.env.names().enumerate() {
            let values = bool_iter.next_variation().unwrap();

            self.write_generated_column(output, values, name);

            if idx != count - 1 {
                output.push(',');
            }
        }
    }

    fn write_checksums(&self, output: &mut String) {
        let checksum = simple_checksum(&output).to_string();
        output.push_str("c=");
        output.push_str(&checksum);
        output.push_str(";\r");

        let checksum = circle_shift_checksum(&output).to_string();
        output.push_str("s=");
        output.push_str(&checksum);
        output.push(';');
    }

    fn write_subcolumns(&self, output: &mut String, subcols_data: &[SubColumnData]) {
        output.push_str("_fTruthColumnExist:");
        output.push_str(subcols_data.len().to_string().as_str());
        output.push('[');

        for (idx, data) in subcols_data.iter().enumerate() {
            data.write(output);
            if idx != subcols_data.len() - 1 {
                output.push(',');
            }
        }
        output.push(']');
    }

    fn write_answer_column(&self, output: &mut String, expr: &str, subcols_data: &[SubColumnData]) {
        output.push_str("openproof.boole.BooleExpressionData=openproof.boole.BooleExpressionData{");
        output.push_str("_fLabelNum=0;_fLabelText=\"\";_fByBoole=false;");

        self.write_subcolumns(output, subcols_data);

        output.push_str("_fExpression=\"");
        output.push_str(expr);
        output.push_str("\";");

        self.write_status_column(output, 1 << self.env.var_count());

        output.push('}');
    }

    fn write_answer_columns(&self, output: &mut String) {
        if let Some(min_degree) = self.write_subexpressions {
            for tree in self.trees {
                let min_degree = tree.root().degree().min(min_degree as usize);

                for node in tree.root().postorder_traversal() {
                    if node.is_var() || node.degree() < min_degree {
                        continue;
                    }

                    let cols = generate_subcolumn_data(node, &self.env);
                    self.write_answer_column(output, node.lpl_formatted().as_str(), &cols);
                    output.push(',');
                }
            }
            // This won't remove a character which is not a ',' because it is guarenteed that the last
            // character is that one
            output.pop();
        } else {
            for tree in self.trees {
                self.write_columns_for_node(output, tree.root());
                output.push(',');
            }
            output.pop();
        }
    }

    fn write_columns_for_node(&self, output: &mut String, node: &ExpressionNode) {
        let cols = generate_subcolumn_data(node, &self.env);
        self.write_answer_column(output, &node.lpl_formatted(), &cols);
    }

    fn write_truth_table_row_status(&self, output: &mut String, is_corrected: bool, num: usize) {
        let c_value = u32::from(is_corrected).to_string();

        output.push_str("openproof.boole.status.TruthTableRowStatus=openproof.boole.status.TruthTableRowStatus{");
        output.push_str("c=");
        output.push_str(&c_value);
        output.push_str(";s=\"\";l=\"\";d@k=\"\";t=false;r=");
        output.push_str(num.to_string().as_str());
        output.push_str(";}");
    }

    fn write_status_column(&self, output: &mut String, num_columns: usize) {
        output.push_str("_fStatusColumn(");
        for num in 0..num_columns {
            self.write_truth_table_row_status(output, true, num);
            output.push(',');
        }
        self.write_truth_table_row_status(output, false, num_columns);
        output.push_str(")o()");
    }

    fn write_all(&self, output: &mut String) {
        self.write_headers(output);

        output.push_str("=openproof.zen.Openproof{");
        output.push_str("p=openproof.boole.Boole{");
        output.push_str("_fAssessmentData=openproof.boole.entities.AssessmentData{");
        output.push_str("_fTitle=@;_fRefData=openproof.boole.entities.ExpressionPanelData{");
        output.push_str("_fExpVector(");

        self.write_generated_columns(output);

        output.push_str(")_fIsReferenceSide=true;"); // fExpVector;
        output.push('}'); // RefData=ExpressionPanelData
        output.push_str("_fSentData=openproof.boole.entities.ExpressionPanelData{");
        output.push_str("_fExpVector(");

        self.write_answer_columns(output);

        output.push_str(")_fIsReferenceSide=false;"); // fExpVector
        output.push('}'); // SentData=ExpressionPanelData
        output.push_str("_fIsTaut=@;_fIsTTPossible=@;_fAreTautEquiv=@;_fIsLastSentenceTautCon=@;");
        output.push_str("_fIsFirstSentenceTautCon=@;_fNeedToBeComplete=@;isContra=@;isTTContra=@;");
        output.push('}'); // _fAssessmentData=openproof.boole.entities.AssessmentData{
        output.push('}'); // p=openproof.boole.Boole{
        output.push('}'); //=openproof.zen.Openproof

        self.write_checksums(output);
    }

    fn generate_string(&self) -> String {
        let mut output = String::with_capacity(self.buffer_capacity);
        self.write_all(&mut output);
        output
    }

    /// Write the contents generated by the generator to the specified writer
    /// The contents are stored beforehand, so the writer does not have to be buffered
    pub fn write_to<W: Write>(&self, mut writer: W) -> Result<(), BoolegenError> {
        let output = self.generate_string();
        writer
            .write_all(output.as_bytes())
            .map_err(BoolegenError::from)
    }

    /// Write the contents generated by the generator to a file named path.
    /// If the file already exists, it will be destroyed
    /// The contents are stored beforehand, so the writer does not have to be buffered
    pub fn write_to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), BoolegenError> {
        self.write_to(File::create(path.as_ref())?)
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
    fn with_capacity(capacity: usize) -> Self {
        Self {
            values: Vec::with_capacity(capacity),
        }
    }

    fn add(&mut self, value: BooleanValue) {
        self.values.push(value);
    }

    pub fn write(&self, output: &mut String) {
        output.push_str("openproof.boole.TruthColumnData=openproof.boole.TruthColumnData{");
        output.push_str("v=\"");
        output.extend(self.values.iter().map(BooleanValue::lpl_boole_encoded));
        output.push_str("\\000\";_fCharIndex=0;_fByBoole=false;");
        output.push('}');
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
