use rocket::local::blocking::Client;
use rocket::http::{RawStr, Status};

#[test]
fn test() {
    let client = Client::tracked(super::rocket()).unwrap();
    let response = client.get("/test").dispatch();
    assert_eq!(response.into_string(), Some("this is a test route! 🧪".into()));
}