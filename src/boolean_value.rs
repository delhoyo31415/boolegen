// I create a new type instead of using bool because I don't
// want the value returned by the operators to be used in the same places where bool is
// allowed (i.e if statements)
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BooleanValue {
    True,
    False,
}

impl BooleanValue {
    pub fn and(&self, other: BooleanValue) -> BooleanValue {
        if *self == BooleanValue::True && other == BooleanValue::True {
            BooleanValue::True
        } else {
            BooleanValue::False
        }
    }

    pub fn or(&self, other: BooleanValue) -> BooleanValue {
        if *self == BooleanValue::True || other == BooleanValue::True {
            BooleanValue::True
        } else {
            BooleanValue::False
        }
    }

    pub fn conditional(&self, other: BooleanValue) -> BooleanValue {
        if *self == BooleanValue::True && other == BooleanValue::False {
            BooleanValue::False
        } else {
            BooleanValue::True
        }
    }

    pub fn biconditional(&self, other: BooleanValue) -> BooleanValue {
        if *self == other {
            BooleanValue::True
        } else {
            BooleanValue::False
        }
    }

    pub fn not(&self) -> BooleanValue {
        if *self == BooleanValue::True {
            BooleanValue::False
        } else {
            BooleanValue::True
        }
    }
}
