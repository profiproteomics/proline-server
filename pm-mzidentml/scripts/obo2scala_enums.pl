#!/usr/bin/env perl

use 5.14.0;
use strict;
use OBO::Parser::OBOParser;

my $onto_prefix = 'Unit';
my $ontology_name = $onto_prefix.'Ontology';
my $enumeration_name = $onto_prefix.'TermId';
my $obo_file = 'unit.obo';

### Parse the OBO file
my $my_parser = OBO::Parser::OBOParser->new();
my $ontology = $my_parser->work($obo_file);
my $onto_id = 'UO';#uc($ontology->default_namespace());
#my $onto_name = defined $ontology->name() ? $ontology->name : '';
#my $onto_url = 'http://purl.obolibrary.org/obo/pato.obo';
#my $onto_url = 'http://psidev.cvs.sourceforge.net/viewvc/*checkout*/psidev/psi/psi-ms/mzML/controlledVocabulary/psi-ms.obo';
#my $onto_version = $ontology->data_version();
#my $onto_date = $ontology->date;

### TODO: export other ontology fields
### TODO: export subsetdef as an enumeration

say <<"END_OF_PACKAGE";
package fr.profi.obo

object $enumeration_name extends Enumeration {
END_OF_PACKAGE

open(FILE,"<",$obo_file);

my $inTerm = 0;
my @terms;

my %cvTerm;
my %all_keys;
while( my $line =<FILE>) {
  
  if( $line =~ /^\[Term\]/ ) {
    $inTerm = 1;
    %cvTerm = ();
  } elsif( $inTerm && $line =~ /^(\w+):\s*(.+)/ ) {
    my( $key, $val ) = ($1,$2);
    push( @{$cvTerm{$key}}, $val );  
    $all_keys{$key} = undef;
    
		if( $key eq 'id' && $val =~ /^$onto_id/ ) {
			my $enum_field = term_id_to_enum_field($val);
			say qq#  val $enum_field = Value("$val")#;
		}
    
  } elsif( $inTerm && $line =~ /^\s*$/ ) {
    $inTerm = 0;
    my %h = %cvTerm;
    push( @terms, \%h);
  }
}

close FILE;

use Data::Dumper;

say <<"END_OF_PACKAGE";
}

object $onto_prefix extends Enumeration {
END_OF_PACKAGE

my %term_name_by_id;
my %term_ids_by_name;
for my $term (@terms) {
  my $isa = defined $term->{is_a} ? $term->{is_a}->[0] : '';
  my $name;
  if( $isa =~ /MS:1001180/ ) {
    $term->{is_regex} = 1;
    my $def = $term->{def}->[0];
    if( $def =~ /"(.+)"/ ) {
      $name = $1;
    }
  } else {
    $name = $term->{name}->[0];
  }
  
  $name =~ s/['!\+\-\?\.\(\)>]//g;
  $name =~ s/\\//g;
  $name =~ s/\///g;
  $name =~ s/@/A/g;
  $name =~ s/[\s:]/_/g;
  $name =~ s/[\[\]\^\*,]//g;	
  my @parts = split("_",$name);
  my $term_name = ucfirst( join( '', map { ucfirst($_) } @parts ) );
  $term_name =~ s/_//g;
  if( $term_name=~ /^\d/ ) {
    $term_name = "N$term_name";
  }
  
  $term->{camelized_name}->[0] = $term_name;
  
  my $id = $term->{id}->[0];
  $term_name_by_id{$id} = $term_name;
  push( @{$term_ids_by_name{$term_name}}, $id );
  
  if( scalar(@{$term_ids_by_name{$term_name}}) > 1 ) {
    $term_name .= '2';
  }
  
  say qq#  val $term_name = Value("$id")#;
}

say "}";

sub term_id_to_enum_field {
	my( $term_id ) = @_;
	
  my $obj_name_suffix = $term_id;
  $obj_name_suffix =~ s/:/_/;
	
	return $obj_name_suffix;
}


1;