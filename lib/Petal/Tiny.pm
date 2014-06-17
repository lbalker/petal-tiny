package Petal::Tiny;
use warnings;
use strict;
use Carp;

# REX/Perl 1.0 
# Robert D. Cameron "REX: XML Shallow Parsing with Regular Expressions",
# Technical Report TR 1998-17, School of Computing Science, Simon Fraser 
# University, November, 1998.
# Copyright (c) 1998, Robert D. Cameron.
# The following code may be freely used and distributed provided that
# this copyright and citation notice remains intact and that modifications
# or additions are clearly identified.
our $TextSE = "[^<]+";
our $UntilHyphen = "[^-]*-";
our $Until2Hyphens = "$UntilHyphen(?:[^-]$UntilHyphen)*-";
our $CommentCE = "$Until2Hyphens>?";
our $UntilRSBs = "[^\\]]*](?:[^\\]]+])*]+";
our $CDATA_CE = "$UntilRSBs(?:[^\\]>]$UntilRSBs)*>";
our $S = "[ \\n\\t\\r]+";
our $NameStrt = "[A-Za-z_:]|[^\\x00-\\x7F]";
our $NameChar = "[A-Za-z0-9_:.-]|[^\\x00-\\x7F]";
our $Name = "(?:$NameStrt)(?:$NameChar)*";
our $QuoteSE = "\"[^\"]*\"|'[^']*'";
our $DT_IdentSE = "$S$Name(?:$S(?:$Name|$QuoteSE))*";
our $MarkupDeclCE = "(?:[^\\]\"'><]+|$QuoteSE)*>";
our $S1 = "[\\n\\r\\t ]";
our $UntilQMs = "[^?]*\\?+";
our $PI_Tail = "\\?>|$S1$UntilQMs(?:[^>?]$UntilQMs)*>";
our $DT_ItemSE = "<(?:!(?:--$Until2Hyphens>|[^-]$MarkupDeclCE)|\\?$Name(?:$PI_Tail))|%$Name;|$S";
our $DocTypeCE = "$DT_IdentSE(?:$S)?(?:\\[(?:$DT_ItemSE)*](?:$S)?)?>?";
our $DeclCE = "--(?:$CommentCE)?|\\[CDATA\\[(?:$CDATA_CE)?|DOCTYPE(?:$DocTypeCE)?";
our $PI_CE = "$Name(?:$PI_Tail)?";
our $EndTagCE = "$Name(?:$S)?>?";
our $AttValSE = "\"[^<\"]*\"|'[^<']*'";
our $ElemTagCE = "$Name(?:$S$Name(?:$S)?=(?:$S)?(?:$AttValSE))*(?:$S)?/?>?";
our $ElemTagCE_Mod = "$S($Name)(?:$S)?=(?:$S)?($AttValSE)";
our $MarkupSPE = "<(?:!(?:$DeclCE)?|\\?(?:$PI_CE)?|/(?:$EndTagCE)?|(?:$ElemTagCE)?)";
our $XML_SPE = "$TextSE|$MarkupSPE";
# REX END - thank you Robert for this 26 line XML parser - awesome ...


our $RE_1 = qr /$ElemTagCE/;
our $RE_2 = qr /$ElemTagCE_Mod/;
our $VARIABLE_RE_SIMPLE   = qq |\$[A-Za-z_][A-Za-z0-9_\.:\/]+|;
our $VARIABLE_RE_BRACKETS = qq |(?<!\$)\\{.*?(?<!\\\\)\\}|;
our $STRING_TOKEN_RE      = "($VARIABLE_RE_SIMPLE|$VARIABLE_RE_BRACKETS)";

our $TAL = 'petal';
our $VERSION = 1.04;

our $STOP_RECURSE = 0;


sub new {
    my $class = shift;
    $class    = ref $class || $class;
    my $thing = shift;
    my $self  = bless {}, $class;
    if    (defined $thing and $thing =~ /(\<|\n|\>)/) { $self->{xmldata} = $thing }
    elsif (defined $thing) {
        $self->{xmldata} = do {
            open XMLDATAFILE, "<$thing" or die "cannot read open $thing";
            my $xmldata = join '', <XMLDATAFILE>;
            close XMLDATAFILE;
            $xmldata;
        };
    }
    return $self;
}


sub process {
    my $self    = shift;
    my $context = { @_ };
    my $data    = $self->{xmldata};
    defined $data or return; # empty data, empty result.
    return makeitso ($data, $context); # earl grey. hot.
}


sub makeitso {
    my $xml     = shift;
    my @xml     = ref $xml ? @{$xml} : ( $xml =~ /$XML_SPE/g );
    my $context = shift || {};
    my @head    = ();
    my @body    = ();
    my @tail    = ();
    while (@xml) {
        my $elem = shift @xml;
        tag_self_close ($elem) and do {
            push @body, $elem;
            @tail = @xml;
            last;
        };
        my $opentag = tag_open ($elem);
        $opentag and do {
            push @body, $elem;
            my $balance = 1;
            while ($balance) {
                @xml or confess "cannot find closing tag for $elem";
                my $elem = shift @xml;
                tag_open  ($elem) and $balance++;
                tag_close ($elem) and $balance--;
                push @body, $elem;
            }
            @tail = @xml;
            last;
        };
        tag_close ($elem) and confess "cannot find opening tag for $elem";

        $elem =~ s/(?<!\$)\$\{?([a-z0-9-\/\:\_]+)\}?/resolve_expression ($1,$context)/egi;
        $elem =~ s/\$\$/\$/g;
        push @head, $elem;
    }
    my @res = ();
    push @res, @head                             if (@head);
    push @res, makeitso_block (\@body, $context) if (@body);
    push @res, makeitso (\@tail, $context)       if (@tail);
    return join '', @res;
}


sub namespace {
    my $node = shift;
    for my $k (keys %{$node}) {
        $k =~ /^xmlns\:/ or next;
        my $v = $node->{$k};
        if ($v eq 'http://purl.org/petal/1.0/') {
            delete $node->{$k};
            $k =~ s/^xmlns\://;
            return $k;
        }
    }
    return;
}


sub makeitso_block {
    my $xml     = shift;
    my $context = shift;
    my @xml     = ref $xml ? @{$xml} : ( $xml =~ /$XML_SPE/g );
    my $tag     = shift (@xml);
    my $gat     = pop (@xml);
    my $node    = tag_open ($tag) || tag_self_close ($tag);
    my $ns      = namespace ($node);
    local $TAL  = $ns || $TAL;
    if (has_instructions ($node)) {
        $context = { %{$context} };
        return tal_on_error ($node, \@xml, $gat, $context);
    }
    else {
        $tag = node2tag ($node) if ($ns);
        if ($gat) { return $tag . makeitso (\@xml, $context) . $gat }
        else      { return $tag                                     } # self-closing tag
    }
}


sub tal_on_error {
    my ($node, $xml, $end, $context) = @_;
    my $stuff   = delete $node->{"$TAL:on-error"};
    defined $stuff or return tal_define ($node, $xml, $end, $context);
    my $nodeCopy = { %{$node} };
    my $res = eval { tal_define ($node, $xml, $end, $context) };
    if ($@) {
        my @result = ();
        for my $k (keys %{$nodeCopy}) { delete $nodeCopy->{$k} if $k =~ /^$TAL:/ }
        delete $nodeCopy->{_close} and $end = "</$nodeCopy->{_tag}>"; # deal with self closing tags
        push @result, node2tag ($nodeCopy);
        push @result, resolve_expression ($stuff, $context);
        push @result, $end;
        return join '', @result;
    }
    else {
        return $res;
    }
}


sub tal_define {
    my ($node, $xml, $end, $context) = @_;
    my $stuff   = delete $node->{"$TAL:define"};
    defined $stuff || return tal_condition ($node, $xml, $end, $context);
    my $newContext = { %{$context} };
    my $define  = trim ($stuff);
    for my $def (split /;(?!;)/, $define) {
        $def = trim($def);
        my ($symbol, $expression) = split /\s+/, $def, 2;
        $newContext->{$symbol} = resolve_expression ($expression, $newContext);
    }
    return tal_condition ($node, $xml, $end, $newContext);
}


sub tal_condition {
    my ($node, $xml, $end, $context) = @_;
    my $stuff   = delete $node->{"$TAL:condition"};
    defined $stuff or return tal_repeat ($node, $xml, $end, $context);

    my $condition = trim ($stuff);
    for my $cond (split /;(?!;)/, $condition) {
        $cond = trim($cond);
        resolve_expression ($condition, $context) or return '';
    }
    return tal_repeat ($node, $xml, $end, $context);
}


sub tal_repeat {
    my ($node, $xml, $end, $context) = @_;
    my $stuff   = delete $node->{"$TAL:repeat"};
    defined $stuff or return tal_content ($node, $xml, $end, $context);
    
    my $repeat = trim ($stuff);
    my ($symbol, $expression) = split /\s+/, $repeat, 2;
    my $array  = resolve_expression ($expression, $context);
    my $count  = 0;
    my @result = ();
    foreach my $item (@{$array}) {
        $count++;
        my $newContext = { %{$context} };
        $newContext->{repeat} = {};
        $newContext->{repeat}->{index}  = $count;
        $newContext->{repeat}->{number} = $count;
        $newContext->{repeat}->{even}   = $count%2 ? 0 : 1;
        $newContext->{repeat}->{odd}    = $count%2 ? 1 : 0;
        $newContext->{repeat}->{start}  = $count == 1 ? 1 : 0;
        $newContext->{repeat}->{end}    = $count == @{$array} ? 1 : 0;
        $newContext->{repeat}->{inner}  = do { ($count > 1 and $count < @{$array}) ? 1 : 0 };
        $newContext->{$symbol} = $item;
        push @result, tal_content ({%{$node}}, $xml, $end, $newContext);
    }
    return join '', @result;
}


sub tal_content {
    my ($node, $xml, $end, $context) = @_;
    my $stuff   = delete $node->{"$TAL:content"};
    defined $stuff or return tal_replace ($node, $xml, $end, $context);
    
    my $res = resolve_expression ($stuff, $context);
    $xml    = defined $res ? [ $res ] : [];
    delete $node->{_close} and $end = "</$node->{_tag}>"; # deal with self closing tags
    
    # set the stop recurse flag so that if content contains $foo and $bar,
    # those aren't interpolated as variables.
    local ( $STOP_RECURSE ) = ( 1 );
    return tal_replace ($node, $xml, $end, $context);
}


sub tal_replace {
    my ($node, $xml, $end, $context) = @_;
    my $stuff   = delete $node->{"$TAL:replace"};
    defined $stuff or return tal_attributes ($node, $xml, $end, $context);
    my $res = resolve_expression ($stuff, $context);
    return defined $res ? $res : '';
}


sub tal_attributes {
    my ($node, $xml, $end, $context) = @_;
    my $stuff = delete $node->{"$TAL:attributes"};
    defined $stuff or return tal_omit_tag ($node, $xml, $end, $context);
   
    my $attributes  = trim ($stuff);
    for my $att (split /;(?!;)/, $attributes) {
        $att = trim ($att);
        my ($symbol, $expression) = split /\s+/, $att, 2;
        $node->{$symbol} = resolve_expression ($expression, $context);
        delete $node->{$symbol} unless (defined $node->{$symbol});
    }
    return tal_omit_tag ($node, $xml, $end, $context);
}


sub tal_omit_tag {
    my ($node, $xml, $end, $context) = @_;
    my $stuff  = delete $node->{"$TAL:omit-tag"};
    my $omit   = defined $stuff ? do { $stuff eq '' ? 1 : resolve_expression ($stuff, $context) } : undef; 
    $omit and not $end and return ''; # omit-tag on a self-closing tag means *poof*, nothing left
    my @result = ();
    push @result, node2tag ($node) unless ($omit);
    if ($end) {
        push @result, do { $STOP_RECURSE ? join '', @{$xml} : makeitso ($xml, $context) };
        push @result, $end unless ($omit);
    }
    return join '', @result;
}


sub resolve_expression {
    my $expr    = trim(shift);
    my $context = shift || confess "resolve_expression() : no context";
    $expr =~ s/\;\;/;/g;
    $expr =~ s/\$\$/\$/g;
    $expr eq 'nothing' and return undef;    
    $expr =~ s/^fresh\s+//;
    my $structure = ($expr =~ s/^structure\s+//);
    return $structure ? resolve ($expr, $context) : xmlencode (resolve ($expr, $context));
}


sub resolve {
    my $expr    = trim(shift);
    my $context = shift || confess "resolve() : no context";
    $expr =~ /:(?!pattern)/ and do {
        my ($mod, $expr) = split /:(?!pattern)/, $expr, 2;
        my $meth = "modifier_$mod";
        Petal::Tiny->can ("modifier_$mod") and return Petal::Tiny->$meth ($expr, $context);
        confess "unknown modifier $mod";
    };
    $expr =~ /^--/ and do {
      $expr =~ s/^--//;
      return $expr;
    };
    $expr =~ s/\r/ /g;
    $expr =~ s/\r/ /g;
    my ($what, @args) = split /\s+/, $expr;
    defined $what or return;
    
    my (@path)   = split /\//, $what;
    my @resolved = ();
    my $obj      = $context;
    while (@path) {
        my $attribute_or_method = shift @path;
        push @resolved, $attribute_or_method;
        my $resolved = join '/', @resolved;
	$obj or confess "cannot fetch $what, because $resolved is undefined";
        ref $obj or confess "cannot fetch $what, because $resolved is not a reference";
        ref $obj eq 'ARRAY' and do {
            not @path and @args and confess "cannot resolve expression $expr";
            $obj = $obj->[$attribute_or_method];
            next;
        };
        ref $obj eq 'HASH' and do {
            not @path and @args and confess "cannot resolve expression $expr";
            $obj = $obj->{$attribute_or_method};            
            next;
        };
        $obj->can ($attribute_or_method) and do {
            if   (@path) { $obj = $obj->$attribute_or_method() }
            else         { $obj = $obj->$attribute_or_method( map { resolve ($_, $context) } @args) }
            next;
        };
        $obj = $obj->{$attribute_or_method};
    }
    return $obj;
}


sub modifier_true {
    my $class = shift;
    my $arg   = resolve (shift(), shift());
    ref $arg and ref $arg eq 'ARRAY' and return @{$arg};
    return $arg ? 1 : 0;
}


sub modifier_false {
    my $class   = shift;
    return not $class->modifier_true (@_);
}


sub modifier_string {
    my $class   = shift;
    my $string  = shift;
    my $context = shift;
    $string     =~ s/(?<!\$)\$\{?([a-z0-9-\/\:\_]+)\}?/resolve ($1,$context)/egi;
    return $string;
}


sub node2tag {
    my $node  = shift;
    my $tag   = delete $node->{_tag};
    for (keys %{$node}) { /^($TAL:)/ and delete $node->{$_} }
    my $open  = delete $node->{_open}  || 0;
    my $close = delete $node->{_close} || 0;
    my $att   = join ' ', map { qq|$_="$node->{$_}"| } keys %{$node};
    $open  and $close and return $att ? "<$tag $att />" : "<$tag />";
    $close and return "</$tag>";
    $open  and return $att ? "<$tag $att>" : "<$tag>";
    die "There is probably a bug somewhere. A tag that's not and open tag and not a close tag?";
}


sub trim {
    my $string = shift;
    $string or return $string;
    $string =~ s/\r//g;
    $string =~ s/\n/ /g;
    $string =~ s/^\s+//;
    $string =~ s/\s+$//;
    return $string;
}


sub has_instructions {
    my $node = shift;
    return grep /^$TAL:/, keys %{$node};
}


sub tag {
    my $elem = shift;
    return tag_open ($elem) || tag_close ($elem) || tag_self_close ($elem);
}


sub tag_open {
    my $elem = shift;
    my $node = undef;
    not defined $elem and confess ('undefined elem');

    $elem !~ /^<\!/ and
    $elem !~ /^<\// and
    $elem !~ /\/>$/ and
    $elem !~ /^<\?/ and
    $elem =~ /^</   and do {
	my %node      = extract_attributes ($elem);
	($node{_tag}) = $elem =~ /.*?([A-Za-z0-9][A-Za-z0-9_:-]*)/;
	$node{_open}  = 1;
	$node{_close} = 0;
	$node = \%node;
    };
    return $node;
}


sub tag_close {
    my $elem = shift;
    my $node = undef;
    $elem !~ /^<\!/ and
    $elem =~ /^<\// and
    $elem !~ /\/>$/ and do {
        my %node      = ();
	($node{_tag}) = $elem =~ /.*?([A-Za-z0-9][A-Za-z0-9_:-]*)/;
	$node{_open}  = 0;
	$node{_close} = 1;
	$node = \%node;
    };
    return $node;
}


sub tag_self_close {
    my $elem = shift;
    my $node = undef;
    $elem !~ /^<\!/ and
    $elem !~ /^<\// and
    $elem =~ /\/>$/ and
    $elem =~ /^</   and do {
	my %node      = extract_attributes ($elem);
        ($node{_tag}) = $elem =~ /.*?([A-Za-z0-9][A-Za-z0-9_:-]*)/;
	$node{_open}  = 1;
	$node{_close} = 1;
	$node = \%node;
    };
    return $node;
}


sub text {
    my $elem = shift; 
    return ($elem !~ /^</) ? $elem : undef;
}


sub extract_attributes {
    my $tag = shift;
    my ($tags) = $tag =~ /$RE_1/g;
    my %attr = $tag =~ /$RE_2/g;
    foreach my $key (keys %attr)
    {
        my $val = $attr{$key};
        $val    =~ s/^(\"|\')//;
        $val    =~ s/(\"|\')$//;
        $attr{$key} = $val;
    }
    
    %attr;
}


sub xmlencode {
    my $string = shift;
    $string or return $string;
    $string =~ s/&/&amp;/g;
    $string =~ s/</&lt;/g;
    $string =~ s/>/&gt;/g;
    $string =~ s/"/&quot;/g;
    $string =~ s/'/&apos;/g;
    return $string;
}


1;


__END__

=head1 NAME

Petal::Tiny - super light TAL for Perl!


=head1 SYNOPSIS

in your Perl code:

  use Petal::Tiny;
  my $template = new Petal::Tiny ('foo.xhtml');
  print $template->process (bar => 'BAZ');


in foo.xhtml

  <html xmlns:tal="http://purl.org/petal/1.0/">
    <body tal:content="bar">Dummy Content</body>
  </html>


and you get something like:

  <html>
    <body>BAZ</body>
  </html>


=head1 SUMMARY

Almost 10 years ago now at the time of this writing, I wrote L<Petal>, an XML
based templating engine that is able to process any kind of XML, XHTML and
HTML. Although I no longer maintain it, I have still used it until today.

L<Petal> is kind of the swiss army knife of the XML templating. It supports
pluggable parsers. Pluggable generators. XML to perl compilation. Disk and
memory caches. Definable charset encoding and decoding. XML or XHTML entity
encoding. I18N. etc. etc.

I wanted something that had most of the really cools feature of L<Petal>, but
that was small and didn't have any dependancies.

Hence, after a couple of days of coding, L<Petal::Tiny> was born. It's still
L<Petal>, but is weighting around 500 lines of code, is completely
self-contained in one .pm file, and doesn't need anything else than Perl.

This POD hence steals a lot of its documentation and explains the differences
between the two modules.


=head1 NAMESPACE

Although this is not mandatory, Petal templates should include use the namespace
L<http://purl.org/petal/1.0/>. Example:

    <html xml:lang="en"
          lang="en"
          xmlns="http://www.w3.org/1999/xhtml"
          xmlns:tal="http://purl.org/petal/1.0/">

      Blah blah blah...
      Content of the file
      More blah blah...
    </html>

If you do not specify the namespace, Petal will by default try to use the
C<petal:> prefix. However, in all the examples of this POD we'll use the
C<tal:> prefix to avoid too much typing.


=head1 KICKSTART

Let's say you have the following Perl code:

    use Petal::Tiny;
    my $template = Petal::Tiny->new ('/my/templates/foo.xml');
    print $template->process ( my_var => some_object() );

some_object() is a subroutine that returns some kind of object, may it be a
scalar, object, array referebce or hash reference. Let's see what we can do...


=head2 Version 1: WYSIWYG friendly prototype.

Using TAL you can do:

    This is the variable 'my_var' :
    <span tal:replace="my_var/hello_world">Hola, Mundo!</span>

Now you can open your template in any WYSIWYG tool (mozilla composer,
frontpage, dreamweaver, adobe golive...) and work with less risk of damaging
your petal commands.


=head2 Version 2: Object-oriented version

Let's now say that C<my_var> is actually an object with a method hello_world()
that returns I<Hello World>. To output the same result, your line, which was:

    <span tal:replace="my_var/hello_world">Hola, Mundo!</span>

Would need to be... EXACTLY the same. Petal lets you access hashes and objects
in an entirely transparent way and tries to automagically do The Right Thing
for you.

This high level of polymorphism means that in most cases you can maintain your
code, swap hashes for objects, and not change a single line of your template
code.


=head2 Version 3: Personalizable

Now let's say that your method hello_world() can take an optional
argument so that C<$some_object-E<gt>hello_world ('Jack')> returns I<Hello Jack>.

You would write:

    <span
        tal:define="var_jack string:Jack"
        tal:replace="my_var/hello_world var_jack">Hola, Mundo!</span>

Optionally, you can directly pass strings (so long as they don't contain
spaces) using two dashes, a la GNU command-line option:

    <span tal:replace="my_var/hello_world --Jack">Hola, Mundo!</span>

TRAP#1: With L<Petal>, You could write:

    <span tal:replace="my_var/hello_world 'Jack'">Hola, Mundo!</span>

This syntax is NOT supported by L<Petal::Tiny>. It's a drag to code, looks ugly
in your templates, and I never used this feature. Thus I dropped it.

TRAP#2: Just like with L<Petal>, you can NOT write nested expressions such as:

    ${my_var/hello_world ${my_var/current_user}}


=head2 Version 4: Internationalized

UNSUPPORTED. Either switch to Petal or write a separate module which handles
this.


=head1 OPTIONS

When you create a L<Petal> template object you can specify plethoras of options
controling file pathes, input parsers / output generators, pluggable encoding
mechanism, language options, etc. etc. Looking back at it I found it totally
over-engineered.

With L<Petal::Tiny> you pass a single argument, which is either a file name or
XML data, and that's it. If the stuff which you pass contains < or a new line,
it's considered XML data. Otherwise it's treated as a file name.


=head1 TAL syntax

Go read L<http://www.zope.org/Wikis/DevSite/Projects/ZPT/TAL>. L<Petal::Tiny>
tries to comply with the TAL spec a lot more than L<Petal> did.

Currently it implements all operations, i.e. define, condition, repeat,
content, replace, attributes, omit-tag and even on-error (which allows for much
nicer error reporting and exception handling than L<Petal>).

But it also tries to remain true to the "Petal Spirit", hence things like
directly interpolating variables still work, so instead of having to type
things such as:

    <!-- fully TAL compliant version -->
    <p>Checkout amount: <span petal:content="self/basket/total">TOTAL</span> USD</p>

You can still write:
    
    <!-- BAM! Petal way. Much easier, especially for quick prototyping. -->
    <p>Checkout amount: $self/basket/total USD</p>

TRAP: Don't forget that the default prefix is C<petal:> NOT C<tal:>, until you
set the petal namespace in your HTML or XML document as follows:

    <html xmlns:tal="http://purl.org/petal/1.0/">


=head1 METAL macros

UNSUPPORTED.


=head1 EXPRESSIONS AND MODIFIERS

Just like L<Petal>, L<Petal::Tiny> has the ability to bind template variables
to the following Perl datatypes: scalars, lists, hash, arrays and objects. The
article describes the syntax which is used to access these from Petal
templates.

In the following examples, we'll assume that the template is used as follows:

  my $hashref = some_complex_data_structure();
  my $template = new Petal::Tiny ('foo.xml');
  print $template->process ( $hashref );

Then we will show how the Petal Expression Syntax maps to the Perl way of
accessing these values.  


=head2 accessing scalar values

Perl expression

  $hashref->{'some_value'};

Petal expression

  some_value

Example

  <!--? Replaces Hello, World with the contents of
        $hashref->{'some_value'}
  -->
  <span tal:replace="some_value">Hello, World</span>


=head2 accessing hashes & arrays

Perl expression

  $hashref->{'some_hash'}->{'a_key'};

Petal expression

  some_hash/a_key

Example

  <!--? Replaces Hello, World with the contents
        of $hashref->{'some_hash'}->{'a_key'}
  -->
  <span tal:replace="some_hash/a_key">Hello, World</span>


Perl expression

  $hashref->{'some_array'}->[12]

Petal expression

  some_array/12

Example

  <!--? Replaces Hello, World with the contents
       of $hashref->{'some_array'}->[12]
  -->
  <span tal:replace="some_array/12">Hello, World</span>

Note: You're more likely to want to loop through arrays:

  <!--? Loops trough the array and displays each values -->
  <ul tal:condition="some_array">
    <li tal:repeat="value some_array"
        tal:content="value">Hello, World</li>
  </ul>


=head2 accessing object methods

Perl expressions

  1. $hashref->{'some_object'}->some_method();
  2. $hashref->{'some_object'}->some_method ('foo', 'bar');
  3. $hashref->{'some_object'}->some_method ($hashref->{'some_variable'})  

L<Petal::Tiny expressions>

  1. some_object/some_method
  2. some_object/some_method --foo --bar
  3. some_object/some_method some_variable

WARNING! The below expressions which work in L<Petal> are UNSUPPORTED by this
module!

  2a. some_object/some_method 'foo' 'bar'
  2b. some_object/some_method "foo" "bar"


=head2 composing

Petal lets you traverse any data structure, i.e.

Perl expression

  $hashref->{'some_object'}
          ->some_method()
          ->{'key2'}
          ->some_other_method ( 'foo', $hash->{bar} );

Petal expression

  some_object/some_method/key2/some_other_method --foo bar


=head2 true:EXPRESSION

  If EXPRESSION returns an array reference
    If this array reference has at least one element
      Returns TRUE
    Else
      Returns FALSE

  Else
    If EXPRESSION returns a TRUE value (according to Perl 'trueness')
      Returns TRUE
    Else
      Returns FALSE

the C<true:> modifiers should always be used when doing Petal conditions.


=head2 false:EXPRESSION

I'm pretty sure you can work this one out by yourself :-)


=head2 set:variable_name EXPRESSION

UNSUPPORTED.


=head2 string:STRING_EXPRESSION

The C<string:> modifier lets you interpolate petal expressions within a string
and returns the value.

  string:Welcome $user/real_name, it is $date!

Alternatively, you could write:

  string:Welcome ${user/real_name}, it is ${date}!
  
The advantage of using curly brackets is that it lets you interpolate
expressions which invoke methods with parameters, i.e.

  string:The current CGI 'action' param is: ${cgi/param --action}

And IMHO, they make your interpolated variables stand out a lot more in your
templates, so I advise you to use them.


=head2 writing your own modifiers

Just go and pollute the Petal::Tiny namespace:

  sub Petal::Tiny::modifier_uppercase {
      my $class   = shift;
      my $string  = shift;
      my $context = shift;
      return uc (Petal::Tiny::resolve ($expression, $context));
  }

Please remember that you need to prefix your modifier name with
'Petal::Tiny::modifier_', thus if you need to create a modifier "SPONGYBOB:",
you define Petal::Tiny::modifier_SPONGYBOB.


=head1 Expression keywords


=head2 XML encoding / structure keyword

By default Petal will encode C<&>, C<<>, C<>> and C<"> to C<&amp;>, C<&lt;>,
C<&gt> and C<&quot;> respectively. However sometimes you might want to display
an expression which is already encoded, in which case you can use the
C<structure> keyword.

  structure my/encoded/variable

Note that this is a language I<keyword>, not a modifier. It does not use a
trailing colon.


=head2 Petal::Hash caching and fresh keyword 

UNSUPPORTED. L<Petal::Tiny> does no caching.


=head2 TOY FUNCTIONS (For debugging or if you're curious)

UNSUPPORTED. Besides, you will find thatL <Petal::Tiny> error reporting and
handling is a lot better than L<Petal>'s, leading to less debugging
requirement. So long as you feed L<Petal::Tiny> with valid XML, you'll be fine.


=head2 UGLY SYNTAX

UNSUPPORTED. See L<Petal::Deprecated>.


=head2 Performance considerations

The cycle of a L<Petal> template is the following:

    1. Read the source XML template
    2. $INPUT (XML or HTML) throws XML events from the source file
    3. $OUTPUT (XML or HTML) uses these XML events to canonicalize the template
    4. Petal::CodeGenerator turns the canonical template into Perl code
    5. Petal::Cache::Disk caches the Perl code on disk
    6. Petal turns the perl code into a subroutine
    7. Petal::Cache::Memory caches the subroutine in memory
    8. Petal executes the subroutine
    9. (optional) Petal internationalizes the resulting output.

If you are under a persistent environement a la mod_perl, subsequent calls to
the same template will be reduced to step 8 until the source template changes.

The cycle of a L<Petal::Tiny> template is the following:

    1. Read the source XML template
    2. Tokenize it using a big regex
    3. Recursively process the tokens

Benchmarking a simple piece of basic XML shows that Petal is much faster when
running its caches, but much slower otherwise:


Benchmark: timing 1000 iterations of Petal (disk cache), Petal (memory cache), Petal (no cache), Petal::Tiny...
Petal (disk cache):  3 wallclock secs ( 2.50 usr +  0.10 sys =  2.60 CPU) @ 384.62/s (n=1000)
Petal (memory cache):  2 wallclock secs ( 1.76 usr +  0.05 sys =  1.81 CPU) @ 552.49/s (n=1000)
Petal (no cache): 18 wallclock secs (17.85 usr +  0.09 sys = 17.94 CPU) @ 55.74/s (n=1000)
Petal::Tiny:  6 wallclock secs ( 6.57 usr +  0.04 sys =  6.61 CPU) @ 151.29/s (n=1000)



=head1 EXPORTS

None.


=head1 BUGS

If you find any, please drop me an email. Patches are always welcome.


=head1 SEE ALSO

L<Petal>, L<Template::TAL>

Jean-Michel Hiver - jhiver (at) gmail (dot) com

This module free software and is distributed under the same license as Perl
itself. Use it at your own risk.
