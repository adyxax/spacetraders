#!/usr/bin/env raku

use lib 'lib';
use JSON::Fast;
use SpaceTraders;

sub MAIN() {
    my SpaceTraders::Client $client .= new;

    CATCH {
        when SpaceTraders::Errors::ExistingContractError { .resume };
        when SpaceTraders::Errors::TokenResetDateMismatchError {
            # wipe db
            $client.register;
            MAIN();
        };
    }

    my $agent = $client.my-agent;
    say $agent.credits;
    my SpaceTraders::Ship @ships = $client.my-ships;

    @ships.map(-> $ship { say $ship.symbol; });

    #my SpaceTraders::Contract $contract = @ships[0].negotiate-contract();
    #dd $contract;
    my SpaceTraders::Contract @contracts = $client.my-contracts;
    say @contracts[0].type;

    say 'all done!';
}
