# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Synapse-Object.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use lib ('../lib', './lib');
use Test::More; # tests => 8;
BEGIN { use_ok('Petal::Tiny') };


my $data   = join '', <DATA>;
my $output = Petal::Tiny::makeitso($data, {
    foo  => 'bar',
    list => [ qw /foo bar baz buz/ ],
} );
like ($output, qr/VISIBLE/, 'visible');
unlike ($output, qr/INVISIBLE/, 'invisible');
like ($output, qr/should be visible/, 'empty quotes');
unlike ($output, qr/strange/, 'crazyness');

Test::More::done_testing();

__DATA__
<XML xmlns:tal="http://purl.org/petal/1.0/">
  <xml tal:condition="false:nothing">VISIBLE</xml>
  <xml tal:condition="true:nothing">INVISIBLE</xml>
  <xml tal:condition="">I guess since omit-tag="" should remove the tag, that means that "" must be true, thus this should be visible as well</xml>
  <xml tal:condition="??">strange stuff</xml>
</XML>
