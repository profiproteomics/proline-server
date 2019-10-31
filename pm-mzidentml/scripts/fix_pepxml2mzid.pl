#!/usr/bin/env perl

use 5.14.0;

use strict;
use Data::Dumper;
use XML::Bare;

our $dir = '\\\\tol-brandir/g$/ABRF';
our @files = qw/
JD_06232014_sample1_A.mzid
JD_06232014_sample2_B.mzid
JD_06232014_sample2_C.mzid
JD_06232014_sample3_A.mzid
JD_06232014_sample3_B.mzid
JD_06232014_sample3_C.mzid
JD_06232014_sample4-A.mzid
JD_06232014_sample4_B.mzid
JD_06232014_sample4_C.mzid
/;

fixMzIdFile($dir . '/' .$_) for @files;

sub fixMzIdFile {
  my $infile = shift;
  
  my $outfile = $infile;
  $outfile =~ s/\.mzid/_fixed.mzid/;
  
  my $searchModByResidue = parseSearchMods($infile);
  my %searchModByUnimodId = map { (split(':',$_->{cvParam}{accession}{value}))[1] => $_ } values(%$searchModByResidue);
  
  #print Dumper $searchMods;
  
  open( IN, '<', $infile) or die $!;
  open( OUT, '>', $outfile) or die $!;
  
  my $isInModBlock = 0;
  my $disableOutput = 0;
  my $pepSeq = '';
  my $monoMass = 0.0;
  my $residue = '';
  my $pepCount = 0;
  
  while( my $line = <IN> ) {
    
    if( $line =~ /<PeptideSequence>(\w+)<\/PeptideSequence>/ ) {
      $pepSeq = $1;
      $pepCount += 1;
    }
    elsif( $line =~ /<Modification location="(\d+)".+monoisotopicMassDelta="(.+?)"/ ) {
      $isInModBlock = 1;
      my $location = $1;
      #$residue = $2;
      $monoMass = $2;
      $residue = $location > 0 ? (split( '', $pepSeq ))[$location - 1] : '.';
      
      if( $location == 1 && ($residue eq 'E' or $residue eq 'Q' or $residue eq 'C') ) {
        $line =~ s/location="1"/location="0"/;
      }
    }
    elsif( $line =~ /<\/Modification>/ ) {
      $isInModBlock = 0;
    }
    elsif( $line =~ /<ModificationParams>/ ) {
      $disableOutput = 1;
      print OUT $line;
    }
    elsif( $disableOutput && $line =~ /<SearchModification fixedMod="true" massDelta="57.021465" residues="C">/ ) {
      print OUT <<"END_OF_MOD";
        <SearchModification fixedMod="false" massDelta="39.995" residues="C">
          <SpecificityRules><cvParam cvRef="MS" accession="MS:1001189" name="modification specificity peptide N-term" value=""/></SpecificityRules>
          <cvParam cvRef="UNIMOD" accession="UNIMOD:26" name="Pyro-carbamidomethyl" value=""/>
        </SearchModification>
END_OF_MOD
  
      $disableOutput = 0;
    }
    elsif( $line =~ /<Enzyme id="ENZ_2"/ ) {
      $disableOutput = 1;
    }
    elsif( $line =~ /<\/Enzymes>/ ) {
      $disableOutput = 0;
    }
    elsif( $line =~ /<\/SearchDatabase>/) {
      print OUT $line if $disableOutput == 0;
      $disableOutput = 1;
    }
    elsif( $line =~ /<SpectraData/) {
      $disableOutput = 0;
    }
    
    if( $isInModBlock ) {
      
      if( $line =~ /<cvParam cvRef="MS" accession="MS:1001460"/ ) {
        #say $pepSeq;
        #say $monoMass;
        #say $residue;
        
        my $mod = $searchModByResidue->{$residue};
        my $modName = $mod->{cvParam}{name}{value};
        my $unimodId = (split(':',$mod->{cvParam}{accession}{value}))[1];
        $line =~ s/cvRef="MS"/cvRef="UNIMOD"/;
        $line =~ s/accession="MS:1001460"/accession="UNIMOD:$unimodId"/;
        $line =~ s/name=".+?"/name="$modName"/;
        
        print OUT $line;
        
      } elsif( $line =~ /<cvParam cvRef="UNIMOD" accession="UNIMOD:(\d+)"/ ) {
        
        my $unimodId = $1;
        
        ### Add workaround for UNIMOD:26 (Pyro-carbamidomethyl)
        if( exists($searchModByUnimodId{$unimodId}) or $unimodId == 26 ) {
          print OUT $line;
        }
      } else {
        print OUT $line;
      }
  
    }
    elsif( $disableOutput == 0 ) {
      print OUT $line;
    }
    
  }
  
  close IN;
  close OUT;

}

sub parseSearchMods {
  my $localInfile = shift;
  
  open( IN, '<', $localInfile) or die $!;
  
  my $modParamsChunk = '';
  my $isInChunk = 0;
  while( my $line = <IN> ) {
    
    if( $line =~ /<ModificationParams>/ ) {
      $isInChunk = 1;      
    }
    
    if( $isInChunk ) {
      $modParamsChunk .= $line;
    }
    
    if( $line =~ /<\/ModificationParams>/ ) {
      $isInChunk = 0;      
    }    
  }
  
  close IN;
  
  my $ob = new XML::Bare( text => $modParamsChunk );
  
  # Parse the xml into a hash tree
  my $xmlTree = $ob->parse();
  
  my %ptmByResidue;
  
  for my $mod (@{$xmlTree->{ModificationParams}{SearchModification}}) {
    if( $mod->{cvParam}{accession}{value} =~ /^UNIMOD/ ) {
      $ptmByResidue{ $mod->{residues}{value} } = $mod;
    }
  }
  
  return \%ptmByResidue;
}

1;