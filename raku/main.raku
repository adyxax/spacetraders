#!/usr/bin/env raku

use lib 'lib';
use JSON::Fast;
use SpaceTraders;
use TOML;

my $state = from-json("state.json".IO.slurp);
my $client = SpaceTraders::Client.new;
my $agent = $client.my-agent;
dd $agent.credits;
