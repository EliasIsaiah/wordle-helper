#[macro_use] extern crate rocket;
use rocket::{serde::{Deserialize, json::Json}, log};

#[cfg(test)] mod tests;

#[derive(Deserialize)]
#[serde(crate = "rocket::serde")]
struct Task<'r> {
    description: &'r str,
    complete: bool
}

#[post("/todo", data = "<task>")]
fn todo(task: Json<Task<'_>>) {
    let message = format!("description: {:?}, complete: {:?}", task.description, task.complete);
    info!("{message}");
    
}

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

#[get("/test")]
fn test() -> &'static str {
    "this is a test route! 🧪"
}


#[get("/hello/<name>/<age>/<cool>")]
fn hello(name: &str, age: u8, cool: bool) -> String {
    if cool {
        format!("You're a cool {} year old, {}!", age, name)
    } else {
        format!("{}, we need to talk about your coolness.", name)
    }
}

#[launch]
fn rocket() -> _ {
    rocket::build()
    .mount("/", routes![test])
    .mount("/", routes![index])
    .mount("/", routes![hello])
    .mount("/", routes![todo])
}
