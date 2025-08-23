unit module SpaceTraders;

use Cro::HTTP::Client;
use JSON::Fast;
use SpaceTraders::Errors;
use URI;
use TOML;

class Agent {...};
class Cargo {...};
class CargoItem {...};
class Cooldown {...};
class Contract {...};
class Delivery {...};
class Fuel {...};
class Nav {...};
class Payment {...};
class Route {...};
class RouteEndpoint {...};
class Ship {...};
class Terms {...};

class Client {
    has Cro::HTTP::Client:D $.client is required;

    #----- Account management --------------------------------------------------
    method my-agent(--> Agent) {
        return Agent.new(self, self.request(:method<GET>, :path</v2/my/agent>));
    }
    method my-contracts(--> Seq) {
        my $data = self.request(:method<GET>, :path</v2/my/contracts>);
        $data.map(-> $contractData { Contract.new(self, $contractData); });
    }
    method my-ships(--> Seq) {
        my $data = self.request(:method<GET>, :path</v2/my/ships>);
        $data.map(-> $shipData {Ship.new(self, $shipData); });
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
        return Agent.new(:client(self), $data<agent>);
    }

    #----- Internals -----------------------------------------------------------
    method new(URI $base-uri = URI.new('https://api.spacetraders.io/v2/') -->Client) {
        my Hash:D $state = from-json("state.json".IO.slurp);
        my Cro::HTTP::Client:D $client .= new(
            base-uri => $base-uri,
            headers => [ authorization => "Bearer {$state<token>}",
                         content-type => 'application/json',
                         host => $base-uri.host,
                       ],
        );
        self.bless: :$client;
    }
    method request(Str :$method, Str :$path, Hash :$payload = {} --> Any) {
        CATCH {
            when X::Cro::HTTP::Error {
                my $json = await .response.body;
                my $err = $json<error>;
                my $apiError = SpaceTraders::Errors::ApiError.new(:code($err<code>) :message($err<message>) :data($err<data>));
                given $err<code> {
                    when 429 {
                        await Promise.in($err<data><retryAfter>);
                        return self.request(:$method :$path :$payload);
                    }
                    when 4113 {
                        die SpaceTraders::Errors::TokenResetDateMismatchError.new($apiError);
                    }
                    when 4511 {
                        die SpaceTraders::Errors::ExistingContractError.new($apiError);
                    }
                }
                die "API Error {$err<code>}: {$err<message>} (data: {to-json $err<data>})";
            }
        }
        my $response = await $!client.request($method, $path, body => $payload);
        my $json = await $response.body;

        return $json<data>;
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
    method new(Client $client, Hash $data --> Agent) {
        self.bless(:$client :accountId($data<accountId>) :credits($data<credits>) :headquarters($data<headquarters>) :shipCount($data<shipCount>)
                   :startingFaction($data<startingFaction>) :symbol($data<symbol>));
    }
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

class Contract {
	has Bool $.accepted;
    has Client $.client;
	has Str $.id;
	has Str $.type;
	has DateTime $.deadlineToAccept;
	has Str $.factionSymbol;
    has Bool $.fulfilled;
    has Terms $.terms;
    method new(Client $client, Hash $data --> Contract) {
        my $deadlineToAccept = DateTime.new($data<deadlineToAccept>);
        my $terms = Terms.new($data<terms>);
        self.bless(:accepted($data<accepted>) :$client :id($data<id>) :type($data<type>) :$deadlineToAccept :factionSymbol($data<factionSymbol>)
                   :fulfilled($data<fulfilled>) :$terms);
    }
}

class Delivery {
    has Str $.destinationSymbol;
    has Str $.tradeSymbol;
    has Int $.unitsFulfilled;
    has Int $.unitsRequired;
    method new(Hash $data --> Delivery) {
        self.bless :destinationSymbol($data<destinationSymbol>) :tradeSymbol($data<tradeSymbol>) :unitsFulfilled($data<unitsFulfilled>) :unitsRequired($data<unitsRequired>);
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

class Payment {
    has Int $.onAccepted;
    has Int $.onFulfilled;
    method new(Hash $data --> Payment) {
        self.bless :onAccepted($data<onAccepted>) :onFulfilled($data<onFulfilled>);
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
    has Client $.client;
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
    method negotiate-contract(-->Contract) {
        Contract.new($.client, $.client.request(:method<POST>, :path("/v2/my/ships/{$.symbol}/negotiate/contract")));
    }
    method new(Client $client, Hash $data --> Ship) {
        my $cargo = Cargo.new($data<cargo>);
        my $cooldown = Cooldown.new($data<cooldown>);
        my $fuel = Fuel.new($data<fuel>);
        my $nav = Nav.new($data<nav>);
        self.bless :$cargo :$client :$cooldown :$fuel :$nav :symbol($data<symbol>);
    }
}

class Terms {
    has DateTime $.deadline;
    has Delivery @.deliver;
    has Payment $.payment;
    method new(Hash $data --> Terms) {
        my $deadline = DateTime.new($data<deadline>);
        my @deliver = $data<deliver>.map(-> $deliveryData { Delivery.new($deliveryData); });
        my $payment = Payment.new($data<payment>);
        self.bless :$deadline :@deliver :$payment;
    }
}
