#!/usr/bin/perl
print "Content-type: text/xml\r\n\r\n";
print "\n";
print "<xml>";
print "<meth>$ENV{REQUEST_METHOD}</meth>\n";
print "<raw>";
while (<STDIN>) {
   print $_;
}
print "</raw>";
print "<env>";
for my $var ( sort keys %ENV ) {
   print "<$var>$ENV{$var}</$var>\n";
}
print "</env>";
print "</xml>\n\n";
