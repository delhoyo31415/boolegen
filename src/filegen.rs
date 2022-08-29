use std::time::SystemTime;

use crate::{
    boolean_value::{BooleanValue, ColumnsBooleanVariations},
    parser::SyntaxTree,
};


pub struct LplBooleGenerator<'a> {
    syntax_tree: &'a SyntaxTree,
    output: String,
}

impl<'a> LplBooleGenerator<'a> {
    pub fn new(syntax_tree: &'a SyntaxTree) -> Self {
        // By default, the buffer will have 4KB
        Self::with_capacity(syntax_tree, 4 * 1024)
    }

    pub fn with_capacity(syntax_tree: &'a SyntaxTree, capacity: usize) -> Self {
        Self {
            syntax_tree,
            output: String::with_capacity(capacity),
        }
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
        self.output += "\rNewFormat\r"
    }

    fn write_generated_column(&mut self, values: &[BooleanValue], name: &str) {
        self.output += "openproof.boole.BooleExpressionData=openproof.boole.BooleExpressionData{";
        self.output += "_fLabelNum=0;_fLabelText=\"\";_fByBoole=true;";
        self.output += "_fTruthColumnExist:1[openproof.boole.TruthColumnData=openproof.boole.TruthColumnData{";
        self.output += "v=\"";
        self.output.extend(values.iter().map(BooleanValue::lpl_boole_encoded));
        self.output += "\\000\";_fCharIndex=0;_fByBoole=true;";
        self.output += "}"; // TruthColumnData
        self.output += "]_fExpression="; // TruthColumnExist
        self.output += name;
        self.output += ";_fStatusColumn@o()";
        self.output += "}"; // BooleExpressionData
    }

    fn write_generated_columns(&mut self) {
        let len = self.syntax_tree.env().len();
        let mut bool_iter = ColumnsBooleanVariations::reversed(len);

        for (idx, name) in self.syntax_tree.env().names_iter().enumerate() {
            let values = bool_iter.next_variation().unwrap();

            // TODO: ensure the name of the variable is allowed by LPL Boole
            self.write_generated_column(values, name);

            if idx != len - 1 {
                self.output += ",";
            }
        }
    }

    fn write_checksums(&mut self) {
        todo!()
    }

    pub fn generate(&mut self) {
        self.write_headers();

        self.output += "=openproof.zen.Openproof{";
        self.output += "p=openproof.boole.Boole{";
        self.output += "_fAssessmentData=openproof.boole.entities.AssessmentData{";
        self.output += "_fTitle=@;_fRefData=openproof.boole.entities.ExpressionPanelData{";
        self.output += "_fExpVector(";
        self.write_generated_columns();
        self.output += ")_fIsReferenceSide=true;"; // fExpVector;
        self.output += "}"; // RefData=ExpressionPanelData
        self.output += "_fIsTaut=@;_fIsTTPossible=@;_fAreTautEquiv=@;_fIsLastSentenceTautCon=@;";
        self.output += "_fIsFirstSentenceTautCon=@;_fNeedToBeComplete=@;isContra=@;isTTContra=@;";
        self.output += "}"; // _fAssessmentData=openproof.boole.entities.AssessmentData{
        self.output += "}"; // p=openproof.boole.Boole{
        self.output += "}"; //=openproof.zen.Openproof
    }

    pub fn into_string(mut self) -> String {
        self.generate();
        self.output
    }
}


impl BooleanValue {
    fn lpl_boole_encoded(&self) -> &'static str {
        match self {
            BooleanValue::True => "T",
            BooleanValue::False => "F"
        }
    }
}