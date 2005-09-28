#!/usr/bin/perl

# Give version-queries.log as STDIN
#
#

use Time::ParseDate; 

use Geo::IP;

my $gi = Geo::IP->new(GEOIP_STANDARD);
 
$today = time;
$oneday = 60*60*24;
$starttime = $today-($oneday*10);

$firstday = 12936;  # manually set: day of introduction

%countries = ();

%users_startups = ();
%users_installtime = (); # first use time
%users_lastcheck = (); # a user's last version check
%new_users_in_period = ();

%checksperday = ();
%checkscurrentperiod = ();

$lastperiod = -1;

# the following assumes sequential reading - 
# earlier events must come earlier
while (<STDIN>)
  {
    # filter garbage at line start - is in there for some reason.. (???)
    if (/^[^A-Z]*\s*[^A-Z]*(.*?)\s\s\s+([\.0-9]+)\s*sess=(\-?[0-9]*)\&.*seq=([0-9]*)\&.*ver=([^\&\n]*)/ig)
      {
	$t = $1;

	$ep = parsedate($t , FUZZY => 1, PREFER_PAST=>1); 
	if (int($ep) < 100)
	  {
	    warn "bad time: ", "x".$t."x". $ep;
	  }
	$starts = $4;
	$ver = $5;
	my $country = $gi->country_name_by_addr($2);
	$countries{$country} += 1;

	$uid = $3;

	# we want to find out:
	# - new users per period, gone-away users per period

	$period = int($ep/($oneday));
	if (exists($users_startups{$uid})) {
	  $users_startups{$uid} += $starts;
	} else {
	  $users_startups{$uid} = $starts;
	  # it's a new user!
	  $new_users_in_period{$period} += 1;
	  $users_installtime{$uid} = $ep;
	}
	$users_lastcheck{$uid} = $ep;
	# conversion rate
	# how many of the new users per period keep using Aquamacs?
    

	# - numbers of users per version with more than 1 request in the last 10 days
	$ver =~ s/%20/ /gs;
	$users_version{$uid} = $ver;

	# checks per day
	if ($period > $lastperiod)
	  {
	   
	    if ($lastperiod > 0)
	      {	   
		$checksperday{$lastperiod} = scalar(keys %checkscurrentperiod);
	      }
	     %checkscurrentperiod = ();
	    $lastperiod = $period;
	  }
	$checkscurrentperiod{$uid} = 1;
      } else {
  # warn "parsing error: ".$_;
}		

  }
print "done1\n";

$checksperday{$lastperiod} = scalar(keys %checkscurrentperiod);

# now check conversion rates

# filter out users that did not stick with Aquamacs
# track versions
$num_users = 0;
%version_dist = {};
%converted_per_period = {};
foreach my $uid (keys %users_installtime)
  {
    if ($users_lastcheck{$uid} - $users_installtime{$uid}>7*$oneday and $users_installtime{$uid}<$today-12*$oneday)
      {
	# user has been sticking with it for at least 10 days
	
	$p = int($users_installtime{$uid}/$oneday);
	$converted_per_period{$p} += 1;
	$num_users += 1;
	$version_dist{$users_version{$uid}} += 1;
      }
  }

 

# print user community stats

print "done2\n";

open F, ">conversionrate.txt";
print F "day  no.users  no.new    no.converted \n";

foreach my $p (sort(map(int, keys(%new_users_in_period))))
  {
    # multiplyu checks per day by 3 --> estimate of user base
    print F $p-$firstday, "\t", 3* $checksperday{$p}, "\t", $new_users_in_period{$p}, "\t";
    if ($converted_per_period{$p})
      {
	print F $converted_per_period{$p}
      } else {
	print F 0
      }
     
    print F "\n";
  }
close F;

print "done3\n";

open F, ">versions.txt";
print F "version   no.users \n" ;

foreach my $v (sort keys %version_dist)
  {
    print F "\"$v\"", "\t", $version_dist{$v}, "\n";
  }
close F;


print "done4\n";
open F, ">countries.txt";
print F "country   no.users \n" ;

foreach my $c (sort keys %countries)
  {
    print F "\"$c\"", "\t", $countries{$c}, "\n";
  }
close F;


# - average startups per user and day (and distribution)

$perday_sum = 0;
$perday_num = 0;
@perday_dist = ();
@usage_duration = ();

foreach my $uid (keys %users_startups)
  {
    # how long has this guy been a user?
    $days = int(($users_lastcheck{$uid} - $users_installtime{$uid}) / $oneday);
    if($days>0)
      {
	$perday = $users_startups{$uid}/$days;
	$perday_sum += $perday;
	$perday_num += 1;
	$perday_dist[int($perday)] += 1;
	$usage_duration[$days] += 1;
      }
  }


open F, ">startups.txt";
print F "no.startups   no.users \n";
print F "# Mean # startups per day: ", ($perday_sum / $perday_num), "\n";
$i=0;
while($i< scalar( @perday_dist))
  {
    print F $i, "\t", $perday_dist[$i], "\n";
    $i++;
  }
close F;

open F, ">usage-duration.txt";
print F "duration   no.users \n"; 

$i=0;
while($i< scalar( @usage_duration))
  {
    print F $i, "\t", $usage_duration[$i], "\n";
    $i++;
  }
close F;

#

print "Number of users (last 10 days): ", $num_users, "\n";

