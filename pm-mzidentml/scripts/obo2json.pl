#!/usr/bin/env perl

use 5.14.0;
use strict;
use Data::Dumper;
use JSON;
use OBO::Parser::OBOParser;

my $onto_prefix = 'Unit';
my $ontology_name = $onto_prefix.'Ontology';
my $enumeration_name = $onto_prefix.'TermId';
my $obo_file = 'unit.obo';

### Parse the OBO file
my $my_parser = OBO::Parser::OBOParser->new();
my $ontology = $my_parser->work($obo_file);
my $onto_id = 'UO'; #uc($ontology->default_namespace());
#my $onto_name = defined $ontology->name() ? $ontology->name : '';
#my $onto_url = 'http://purl.obolibrary.org/obo/pato.obo';
#my $onto_url = 'http://psidev.cvs.sourceforge.net/viewvc/*checkout*/psidev/psi/psi-ms/mzML/controlledVocabulary/psi-ms.obo';
#my $onto_version = $ontology->data_version();
#my $onto_date = $ontology->date;

my @obo_terms = @{$ontology->get_terms()}; # get all the terms

### Parse the OBO file into RAW hashes (in order to fix issues with OBOParser)
open(FILE,"<",$obo_file);

my $inTerm = 0;
my %raw_term_by_id;
my %raw_term;

while( my $line =<FILE>) {
	
  if( $line =~ /^\[Term\]/ ) {
    $inTerm = 1;
    %raw_term = ();
  } elsif( $inTerm && $line =~ /^(\w+):\s*(.+)/ ) {
    my( $key, $val ) = ($1,$2);
    push( @{$raw_term{$key}}, $val );
  } elsif( $inTerm && $line =~ /^\s*$/ ) {
    $inTerm = 0;
    my %h = %raw_term;
    $raw_term_by_id{ $raw_term{id}->[0] } = \%h;
  }
}

close FILE;

my @term_hashes;
foreach my $t (@obo_terms) {
	
	if( $t->id =~ /^$onto_id/ ) {
		my $term_as_hash = term2hash( $t, $raw_term_by_id{$t->id} );		
		push(@term_hashes, $term_as_hash );
	}
  
  #print "The name of the term is: ", $t->name(), ", and its ID is:", $t->id(), "\n";
}

say JSON->new->pretty->encode( \@term_hashes );

sub term2hash {
  my( $term, $raw_term ) = @_;
  
  my $id = $term->id();
  my $name = defined $term->name() ? $term->name : '';
	$name =~ s/\t/ /g;
    # WARNING: this has been commented because it causes issues for X!Tandem
    # Note that we edited manually the psi-ms_terms.json to remove the \ from X\!Tandem
	#$name =~ s/\\/\\\\/g;
	
	my $def = '';
  if( defined $term->def->text ) {
		$def = $term->def->text;
		$def =~ s/\"/&quote/g;
		#$def =~ s/\\/\\\\/g;
		$def =~ s/&quote/\\"/g;
	}
	
	my %term_hash;
	$term_hash{id} = $id;
	$term_hash{name} = $name;
	$term_hash{definition} = $def;

  if( $term->is_anonymous ) {
		$term_hash{is_anonymous} = JSON::true;
  }
  
  if( defined $raw_term->{alt_id} ) {
		$term_hash{alt_ids} = $raw_term->{alt_id};
  }

  if( defined $term->comment ) {
    my $comment = $term->comment;
		$comment =~ s/"/\\"/g;
		$term_hash{comment} = $comment;
  }
  
  if( defined $term->subset ) {
		$term_hash{subsets} = [ $term->subset ];
  }
  
  if( defined $term->synonym_set ) {
    
    my $make_synonym_hash = sub {
      my $synonym = $_[0];

			my %synonym_hash;
			$synonym_hash{scope} = $synonym->scope;
			$synonym_hash{definition} = $synonym->def->text;
			
      if( defined $synonym->synonym_type_name ) {
				$synonym_hash{type_name} = $synonym->synonym_type_name;
		  }
			
			### TODO: parse dbXRefs using raw term
			#my $db_xrefs = 'Array()';
			
			return \%synonym_hash;
    };
		
    my @synonyms = map { $make_synonym_hash->($_) } $term->synonym_set;
    
		$term_hash{synonyms} = \@synonyms;
  }
  
  if( defined $term->xref_set->get_set ) {
		
		my @xrefs;
		for my $xref ($term->xref_set->get_set) {
			
			my %xref;
			$xref{db} = $xref->db;
			
			my $ac = $xref->acc;
			$ac =~ s/\\//g;
			$xref{ac} = $ac;
			
			$xref{description} = $xref->description;
			$xref{modifier} = $xref->modifier if length($xref->modifier) > 0;
			
			push( @xrefs, \%xref );
		}
		
		$term_hash{xrefs} = \@xrefs;
  }
	
	my( @is_a, @rel_as_hashes );
	my $relationships = $ontology->get_relationships_by_source_term($term);
	
  if( defined $relationships ) {
		for my $relationship (@$relationships) {
			my $type = $relationship->type;
			my $id = $relationship->head->id;
			if( $type eq 'is_a' ) {
				push( @is_a, $id );
			} else {
				push( @rel_as_hashes, { link_type => $type, target => $id } );
			}
		}
  }
  
  if( scalar(@is_a) > 0 ) {
		$term_hash{is_a} = \@is_a;
  }
  
  if( defined $term->{intersection_of} ) {
  }
  
  if( defined $term->{union_of} ) {
  }
  
  if( defined $term->{disjoint_from} ) {
  }
  
  if( scalar(@rel_as_hashes) > 0 ) {
		$term_hash{relationships} = \@rel_as_hashes;
  }
  
  if( $term->is_obsolete ) {
		$term_hash{is_obsolete} = JSON::true;
  }
  
  if( defined $raw_term->{replaced_by} ) {
		$term_hash{replaced_by} = $raw_term->{replaced_by};
  }
  
  if( defined $raw_term->{consider} ) {
		$term_hash{consider} = $raw_term->{consider};
  }
  
  if( defined $term->{created_by} ) {
    
  }
  
  if( defined $term->{creation_date} ) {
    
  }	

  return \%term_hash;
}

1;