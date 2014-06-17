# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Synapse-Object.t'
# fixed on-error bug
use strict;
use lib ('../lib', './lib');
use Test::More tests => 2;
BEGIN { use_ok('Petal::Tiny') };
package MockObject;

sub cgi   { my $self = shift; $self->{cgi} ||= CGI->new(); $self->{cgi} };
sub cli   { return { Result => 100 } }
sub Debug { my $self = shift; use Data::Dumper; return "<pre>" . Dumper ($self, @_) . "</pre>" } 


package main;
use CGI;

my $mock = bless { cgi => CGI->new() }, "MockObject";
$mock->cgi()->param (pcode => 'dje');

my $data = join '', <DATA>;
my $t = Petal::Tiny->new ($data);
my $out = $t->process (self => $mock);
like ($out, qr /100/);

__DATA__
<xml petal:on-error="string:foo" petal:define="pcode self/cgi/param --pcode; query self/cli --type pcode --count" petal:content="query/Result">dfksd</xml>
