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
        excluded_indices: Option<Vec<i8>>,
    }

    impl Letter {
        pub fn new(value: char, position: i8, excluded_indices: Option<Vec<i8>>) -> Self {
            Letter { value, position, excluded_indices }
        }
    }

    pub fn get_words_with_matching_letters(allPossibleWords: Vec<&str>, guessedWordVector: &Vec<Letter>) -> Vec<String> {
        // find words containing the letters
        // then weed out the words that don't have letters in the correct position
        let mut result: Vec<String> = vec![];
        for word in allPossibleWords {
            // let wordCharset = &word.chars();
            for guessLetter in guessedWordVector {
                for (i, ith_char) in word.clone().chars().enumerate() {
                    
                    if guessLetter.position < 0 { // yellow
                        if ith_char == guessLetter.value {
                            result.push(String::from(word));
                        }
                    } else { // green
                       if i as i8 == guessLetter.position {
                        if ith_char == guessLetter.value {
                            result.push(String::from(word));
                        }
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

pub fn get_filtered_words(allPossibleWords: &Vec<String>, excludedChars: Vec<char>) -> Vec<String> {
    let mut filtered_words = allPossibleWords.clone();
    for word in allPossibleWords {
        let split_string = word.chars();
        if split_string.enumerate().any(|c| excludedChars.contains(&c.1)) {
            filtered_words = filtered_words
            .into_iter()
            .filter(|s| !s.parse::<String>().unwrap().eq_ignore_ascii_case(word))
            .collect();
        }
    }
    filtered_words
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
        let guesses = vec![Letter::new('s', 0, None), Letter::new('t', -1, Some(vec![2]))];
        let expected = vec!["smite".to_string(), "smote".to_string()];
        let actual = get_words_with_matching_letters(words, &guesses); // stool
        let result_one = actual.iter().any(|x| x == "smote");
        let result_two = actual.iter().any(|x| x == "smite");
        assert!(result_one && result_two);
    }

    #[test]
    fn filter_out_words_based_on_char_exclusion_list() {
        let char_exclusion_list = vec!['a','m','d','y'];
        let words: Vec<String> = vec!["tooth".to_string(), "patio".to_string(), "alien".to_string(), "smite".to_string(), "sugar".to_string(), "smote".to_string()];
        let expected = vec!["tooth".to_string()];
        let actual = get_filtered_words(&words, char_exclusion_list);
        assert_eq!(expected, actual);
    }
}
