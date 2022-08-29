use std::fmt::Display;

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

impl Display for BooleanValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BooleanValue::True => "True".fmt(f),
            BooleanValue::False => "False".fmt(f),
        }
    }
}

// This is an example where GATs and a StreamingIterator or LendingIterator
// would be really helpful
#[derive(Debug, Clone)]
pub struct BooleanVariations {
    // The current variation
    var: Vec<BooleanValue>,
    // Auxilar variable to calculate the next variation
    bin: Vec<usize>,
    bool_value_for_zero: BooleanValue,
}

impl BooleanVariations {
    pub fn new(len: usize) -> Self {
        Self::with_starting_value(len, BooleanValue::False)
    }

    pub fn reversed(len: usize) -> Self {
        Self::with_starting_value(len, BooleanValue::True)
    }

    fn with_starting_value(len: usize, bool_value: BooleanValue) -> Self {
        if len == 0 {
            panic!("len cannot be 0");
        }

        let bits = usize::BITS as usize;
        let entries = len / bits;
        let entries = if len % bits == 0 {
            entries
        } else {
            entries + 1
        };

        Self {
            var: vec![bool_value; len],
            bin: vec![0; entries],
            bool_value_for_zero: bool_value,
        }
    }

    pub fn next_variation<'a>(&'a mut self) -> Option<&'a [BooleanValue]> {
        if self.has_finished() {
            return None;
        }

        let mut idx = 0;
        let mut current_bin = self.bin[idx];

        for bool_value in self.var.iter_mut().rev() {
            *bool_value = if current_bin & 1 == 1 {
                self.bool_value_for_zero.not()
            } else {
                self.bool_value_for_zero
            };

            current_bin >>= 1;
            if current_bin == 0 {
                idx += 1;
                if idx < self.bin.len() {
                    current_bin = self.bin[idx];
                }
            }
        }
        self.update_bin();

        Some(&self.var)
    }

    fn update_bin(&mut self) {
        for num in &mut self.bin {
            let (new_num, overflowed) = num.overflowing_add(1);
            *num = new_num;
            if !overflowed {
                break;
            }
        }
    }

    pub fn has_finished(&self) -> bool {
        self.var
            .iter()
            .all(|&bool_val| bool_val == self.bool_value_for_zero.not())
    }
}

#[derive(Debug, Clone)]
pub struct ColumnsBooleanVariations {
    column: Vec<BooleanValue>,
    step: usize,
    starting_value: BooleanValue,
}

impl ColumnsBooleanVariations {
    pub fn new(len: usize) -> Self {
        Self::with_starting_value(len, BooleanValue::False)
    }

    pub fn reversed(len: usize) -> Self {
        Self::with_starting_value(len, BooleanValue::True)
    }

    pub fn next_variation<'a>(&'a mut self) -> Option<&'a [BooleanValue]> {
        if self.has_finished() {
            return None;
        }

        let mut current_value = self.starting_value;
        for (idx, bool_value) in self.column.iter_mut().enumerate() {
            if idx % self.step == 0 {
                current_value = current_value.not();
            }
            *bool_value = current_value;
        }

        self.step >>= 1;

        Some(&self.column)
    }

    pub fn has_finished(&self) -> bool {
        self.step == 0
    }

    fn with_starting_value(len: usize, starting_value: BooleanValue) -> Self {
        // TODO: The memory consumption for values of len of at least 30 is HUGE (at least 1GiB)
        // Think if there is a better way (I doubt because of the exponential nature of this calculation)
        // Maybe the row values can be output from an iterator
        Self {
            column: vec![BooleanValue::False; 1 << len],
            step: 1 << (len - 1),
            starting_value: starting_value.not(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn variation_equals(iter: &mut BooleanVariations, expected: &[BooleanValue]) {
        assert_eq!(iter.next_variation().unwrap(), expected)
    }

    #[test]
    fn boolean_value_variations_correctly_generated() {
        let mut iter = BooleanVariations::new(3);

        variation_equals(
            &mut iter,
            &[
                BooleanValue::False,
                BooleanValue::False,
                BooleanValue::False,
            ],
        );
        variation_equals(
            &mut iter,
            &[BooleanValue::False, BooleanValue::False, BooleanValue::True],
        );
        variation_equals(
            &mut iter,
            &[BooleanValue::False, BooleanValue::True, BooleanValue::False],
        );
        variation_equals(
            &mut iter,
            &[BooleanValue::False, BooleanValue::True, BooleanValue::True],
        );
        variation_equals(
            &mut iter,
            &[BooleanValue::True, BooleanValue::False, BooleanValue::False],
        );
        variation_equals(
            &mut iter,
            &[BooleanValue::True, BooleanValue::False, BooleanValue::True],
        );
        variation_equals(
            &mut iter,
            &[BooleanValue::True, BooleanValue::True, BooleanValue::False],
        );
        variation_equals(
            &mut iter,
            &[BooleanValue::True, BooleanValue::True, BooleanValue::True],
        );

        assert_eq!(iter.next_variation(), None);
    }

    #[test]
    fn boolean_value_variations_reversed() {
        let mut iter = BooleanVariations::reversed(2);

        variation_equals(&mut iter, &[BooleanValue::True, BooleanValue::True]);
        variation_equals(&mut iter, &[BooleanValue::True, BooleanValue::False]);
        variation_equals(&mut iter, &[BooleanValue::False, BooleanValue::True]);
        variation_equals(&mut iter, &[BooleanValue::False, BooleanValue::False]);

        assert_eq!(iter.next_variation(), None);
    }

    #[test]
    fn column_boolean_value_variations_correctly_generated() {
        let mut iter = ColumnsBooleanVariations::new(2);

        assert_eq!(
            iter.next_variation().unwrap(),
            &[
                BooleanValue::False,
                BooleanValue::False,
                BooleanValue::True,
                BooleanValue::True
            ]
        );
        assert_eq!(
            iter.next_variation().unwrap(),
            &[
                BooleanValue::False,
                BooleanValue::True,
                BooleanValue::False,
                BooleanValue::True
            ]
        );

        assert_eq!(iter.next_variation(), None);
    }

    #[test]
    fn column_boolean_value_variations_reversed() {
        let mut iter = ColumnsBooleanVariations::reversed(2);

        assert_eq!(
            iter.next_variation().unwrap(),
            &[
                BooleanValue::True,
                BooleanValue::True,
                BooleanValue::False,
                BooleanValue::False
            ]
        );
        assert_eq!(
            iter.next_variation().unwrap(),
            &[
                BooleanValue::True,
                BooleanValue::False,
                BooleanValue::True,
                BooleanValue::False
            ]
        );
    }
}
