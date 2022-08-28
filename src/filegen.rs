use core::time;
use std::time::SystemTime;

use crate::parser::SyntaxTree;

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
            .expect("Time went before epoch")
            .as_millis();

        // Timestamp when the file is saved
        // TODO: this should be randomized or chosen by the user
        let close_timestamp = open_timestamp + 3 * 60 + 17;

        self.output += open_timestamp.to_string().as_str();
        self.output += "D";
        self.output += close_timestamp.to_string().as_str();
        self.output += "\rNewFormat\r"
    }

    fn write_truth_value_variation(&mut self) {
        todo!()
    }

    fn write_truth_value_variations(&mut self) {
        todo!()
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
        self.write_truth_value_variations();
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
