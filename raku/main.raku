#!/usr/bin/env raku

use lib 'lib';
use JSON::Fast;
use SpaceTraders;
use TOML;

my $state = from-json("state.json".IO.slurp);
my SpaceTraders::Client $client .= new;
my $agent = $client.my-agent;
my SpaceTraders::Ship @ships = $client.my-ships;
CATCH {
    when SpaceTraders::Errors::ExistingContractError { .resume };
}
my SpaceTraders::Contract $contract = @ships[0].negotiate-contract();
dd $contract;
my SpaceTraders::Contract @contracts = $client.my-contracts;
dd @contracts;
