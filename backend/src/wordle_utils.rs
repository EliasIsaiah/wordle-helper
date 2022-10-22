mod wordle_utils {
    use std::{
        fs::File,
        io::{BufRead, BufReader},
    };

    pub fn get_file_data() -> Vec<String> {
        let file = File::open("wordle_list_all.txt").expect("file wasn't found.");
        let reader = BufReader::new(file);
        reader.lines().map(|line| line.unwrap()).collect()
    }

    pub struct Letter {
        value: String,
        position: i8,
    }

    impl Letter {
        pub fn new(value: String, position: i8) -> Self {
            Letter { value, position }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::wordle_utils::wordle_utils::*;

    #[test]
    fn get_words() {
        let expected = "aback".to_ascii_uppercase();
        let actual = get_file_data();
        assert_eq!(expected, actual[0].to_ascii_uppercase());
    }

    #[test]
    fn get_words_with_letters_in_matching_locations() {
        let words = vec!["tooth".to_string(), "patio".to_string(), "alient".to_string(), "smite".to_string(), "sugar".to_string(), "smote".to_string()];
        let guess = [
            Letter::new(String::from("s"), 0),
            Letter::new(String::from("t"), -1),
        ];
        let expected = vec!["smite".to_string(), "smote".to_string()];
        assert!(expected.contains(&"smote".to_string()));
    }
}
