unit module SpaceTraders::Errors;

class ApiError is Exception {
    has Int $.code is required;
    has Str $.message is required;
    has Any $.data is required;
    method gist() {
        "API Error code {$.code}: {$.message}\n{to-json $.data}";
    }
}

class TokenResetDateMismatchError is Exception {
    has Str $.actual is required;
    has Str $.expected is required;
    method gist() {
        "Failed to parse token. Token reset_date does not match the server. Server resets " ~
        "happen on a weekly to bi-weekly frequency during alpha. After a reset, you should " ~
        "re-register your agent. actual: {$.actual} expected: {$.expected}";
    }
    method new(ApiError $err) {
        self.bless :actual($err.data<actual>) :expected($err.data<expected>);
    }
}
