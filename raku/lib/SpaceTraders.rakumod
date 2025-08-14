unit module SpaceTraders;

use HTTP::UserAgent;
use JSON::Fast;
use SpaceTraders::Errors;
use URI;
use TOML;

class Agent {...};

class Client {
    has URI $.base-uri;
    has HTTP::Header $.headers;
    has HTTP::UserAgent $.ua = HTTP::UserAgent.new;

    #----- Account management --------------------------------------------------
    method my-agent(--> Agent) {
        return Agent.new(:client(self), |self.request(:method<GET>, :path</v2/my/agent>));
    }
    method register(--> Agent) {
        my $config = from-toml("config.toml".IO.slurp);
        $.headers.field(authorization => "Bearer {$config<account><token>}");
        my %payload = (
            faction => $config<agent><faction>,
            symbol  => $config<agent><symbol>,
        );
        my $data = self.request(:method<POST> :path</v2/register> :%payload);
        spurt "state.json", to-json($data);
        $.headers.field(authorization => "Bearer {$data<token>}");
        return Agent.new(:client(self), |$data<agent>);
    }

    #----- Internals -----------------------------------------------------------
    method new(URI $base-uri = URI.new('https://api.spacetraders.io/v2/') -->Client) {
        my $state = from-json("state.json".IO.slurp);
        my $headers = HTTP::Header.new :authorization("Bearer {$state<token>}")
                                       :content-type<application/json>
                                       :host($base-uri.host);
        self.bless :$base-uri :$headers;
    }
    method request(Str :$method, Str :$path, Hash :$payload) {
        my $uri = $!base-uri.clone;
        $uri.path($path);
        my $request = HTTP::Request.new($method, $uri, $!headers);
        $request.add-content(to-json $payload) if $payload;
        my $res = $!ua.request($request);
        my $content = $res.content;
        my $parsed = try from-json $content;
        unless $parsed {
            die "Invalid JSON response: $content";
        }

        if $res.is-success {
            return $parsed<data>;
        }
        else {
            my $err = $parsed<error>;
            my $apiError = SpaceTraders::Errors::ApiError.new
                           :code($err<code>) :message($err<message>) :data($err<data>);
            given $err<code> {
                when 4113 {
                    die SpaceTraders::Errors::TokenResetDateMismatchError.new($apiError);
                }
            }
            die "API Error {$err<code>}: {$err<message>} (data: {to-json $err<data>})";
        }
    }
}

#----- Entities ----------------------------------------------------------------
class Agent {
	has Str $.accountId;
    has Client $.client;
	has Int $.credits;
	has Str $.headquarters;
	has Int $.shipCount;
	has Str $.startingFaction;
	has Str $.symbol;
}
