# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Synapse-Object.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use lib ('../lib', './lib');
use Test::More tests => 8;
BEGIN { use_ok('Petal::Tiny') };

my $data = join '', <DATA>;
my $output = Petal::Tiny::makeitso($data, {
    foo  => 'bar',
    list => [ qw /one two three four/ ],
    keys => [ qw /foo/ ],
    hash => { foo => "fooval", bar => "barval" },
    nums => [ 1, 0 ],
    array => [ qw/ val0 val1 val2 / ],
} );

like ($output, qr/one/, 'one');
like ($output, qr/two/, 'two');
like ($output, qr/three/, 'three');
like ($output, qr/four/, 'four');
like ($output, qr/>fooval</, 'fooval');
unlike ($output, qr/barval/, 'barval');
like ($output, qr/val1.*?val0/s, 'val1 followed by val0');

Test::More::done_testing();

__DATA__
<XML xmlns:tal="http://purl.org/petal/1.0/">
  <xml tal:repeat="item list">
    <span tal:attributes="content item; bar foo" tal:content="item">foo</span>
    <span tal:replace="item">foo</span>
    <p>repeat/index: <span tal:replace="repeat/index">index</span></p>
    <p>repeat/number: <span tal:replace="repeat/number">number</span></p>
    <p>repeat/even: <span tal:replace="repeat/even">even</span></p>
    <p>repeat/odd: <span tal:replace="repeat/odd">odd</span></p>
    <p>repeat/start: <span tal:replace="repeat/start">start</span></p>
    <p>repeat/end: <span tal:replace="repeat/end">end</span></p>
    <p>repeat/inner: <span tal:replace="repeat/inner">inner</span></p>
  </xml>
  <xml tal:repeat="key keys" tal:content="hash key"/>
  <xml tal:repeat="num nums" tal:content="array num"/>
</XML>
