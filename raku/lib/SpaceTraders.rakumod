unit module SpaceTraders;

use HTTP::UserAgent;
use JSON::Fast;
use SpaceTraders::Errors;
use URI;
use TOML;

class Agent {...};
class Cargo {...};
class CargoItem {...};
class Cooldown {...};
class Fuel {...};
class Nav {...};
class Route {...};
class RouteEndpoint {...};
class Ship {...};

class Client {
    has URI $.base-uri;
    has HTTP::Header $.headers;
    has HTTP::UserAgent $.ua = HTTP::UserAgent.new;

    #----- Account management --------------------------------------------------
    method my-agent(--> Agent) {
        return Agent.new(:client(self), |self.request(:method<GET>, :path</v2/my/agent>));
    }
    method my-ships(--> Seq) {
        my $data = self.request(:method<GET>, :path</v2/my/ships>);
        $data.map(-> $shipData {Ship.new($shipData); });
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
        my $uri = $.base-uri.clone;
        $uri.path($path);
        my $request = HTTP::Request.new($method, $uri, $.headers);
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

class Cargo {
    has Int $.capacity;
    has CargoItem @.inventory;
    has Int $.units;
    method new(Hash $data --> Cargo) {
        my @inventory = $data<inventory>.elems > 0 ?? $data<inventory>.map(CargoItem.new($_)) !! ();
        self.bless :capacity($data<capacity>) :@inventory :units($data<units>);
    }
}

class CargoItem {
    has Str $.description;
    has Str $.name;
    has Str $.symbol;
    has Int $.units;
    method new(Hash $data --> CargoItem) {
        self.bless |$data;
    }
}

class Cooldown {
    has Str $.shipSymbol;
    has Int $.totalSeconds;
    has Int $.remainingSeconds;
    method new(Hash $data --> Cooldown) {
        self.bless :shipSymbol($data<shipSymbol>) :totalSeconds($data<totalSeconds>) :remainingSeconds($data<remainingSeconds>);
    }
}

class Fuel {
    has Int $.capacity;
    # consumed
    has Int $.current;
    method new(Hash $data --> Fuel) {
        self.bless :capacity($data<capacity>) :current($data<current>);
    }
}

class Nav {
    has Str $.flightMode;
    has Route $.route;
    has Str $.status;
    has Str $.systemSymbol;
    has Str $.waypointSymbol;
    method new(Hash $data --> Nav) {
        my $route = Route.new($data<route>);
        self.bless :flightMode($data<flightMode>) :$route :status($data<status>) :systemSymbol($data<systemSymbol>) :waypointSymbol($data<waypointSymbol>);
    }
}

class Route {
    has DateTime $.arrival;
    has DateTime $.departureTime;
    has RouteEndpoint $.destination;
    has RouteEndpoint $.origin;
    method new(Hash $data --> Route) {
        my $arrival = DateTime.new($data<arrival>);
        my $departureTime = DateTime.new($data<departureTime>);
        my $destination = RouteEndpoint.new($data<destination>);
        my $origin = RouteEndpoint.new($data<origin>);
        self.bless :$arrival :$departureTime :$destination :$origin;
    }
}

class RouteEndpoint {
    has Str $.type;
    has Str $.symbol;
    has Str $.systemSymbol;
    has Int $.x;
    has Int $.y;
    method new(Hash $data --> RouteEndpoint) {
        self.bless :type($data<type>) :symbol($data<symbol>) :systemSymbol($data<systemSymbol>) :x($data<x>) :y($data<y>);
    }
}

class Ship {
    has Cargo $.cargo;
    has Cooldown $.cooldown;
    # crew
    # engine
    # frame
    has Fuel $.fuel;
    # modules
    # mounts
    has Nav $.nav;
    # reactor
    # registration
    has Str $.symbol;
    method new(Hash $data --> Ship) {
        my $cargo = Cargo.new($data<cargo>);
        my $cooldown = Cooldown.new($data<cooldown>);
        my $fuel = Fuel.new($data<fuel>);
        my $nav = Nav.new($data<nav>);
        self.bless :$cargo :$cooldown :$fuel :$nav :symbol($data<symbol>);
    }
}
