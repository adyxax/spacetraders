unit module SpaceTraders::Errors;

use JSON::Fast;

class ApiError is Exception {
    has Int $.code is required;
    has Str $.message is required;
    has Any $.data is required;
    method gist() {
        "API Error code {$.code}: {$.message}\n{to-json $.data}";
    }
}

role SimpleError is Exception {
    has ApiError $.error;
    method gist() {
        $.error.gist;
    }
    method new(ApiError $error) {
        self.bless :$error;
    }
}

class ExistingContractError does SimpleError {}
class TokenResetDateMismatchError does SimpleError {}
