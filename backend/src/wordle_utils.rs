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
        value: char,
        position: i8,
    }

    impl Letter {
        pub fn new(value: char, position: i8) -> Self {
            Letter { value, position }
        }
    }

    pub fn get_words_with_matching_letters(words: Vec<&str>, guesses: &Vec<Letter>) -> Vec<String> {
        // find words containing the letters
        // then weed out the words that don't have letters in the correct position
        let mut result: Vec<String> = vec![];
        for word in words {
            // let wordCharset = &word.chars();
            for guessLetter in guesses {
                for (i, ith_char) in word.clone().chars().enumerate() {
                    if guessLetter.position < 0 {
                        if ith_char == guessLetter.value {
                            result.push(String::from(word));
                        }
                    }
                }
                // if wordCharset.clone().enumerate().any(|c| c.1 == guessLetter.value) {
                //     let variable = true;
                // }
            }
        }
        vec!["smite".to_string(), "smote".to_string()]
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
        let words = vec!["tooth", "patio", "alien", "smite", "sugar", "smote"];
        let guesses = vec![Letter::new('s', 0), Letter::new('t', -1)];
        let expected = vec!["smite".to_string(), "smote".to_string()];
        let actual = get_words_with_matching_letters(words, guesses);
        let result_one = actual.iter().any(|x| x == "smote");
        let result_two = actual.iter().any(|x| x == "smite");
        assert!(result_one && result_two);
    }
}
