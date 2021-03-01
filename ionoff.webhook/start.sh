#!/bin/bash


AWK=awk
if [[ "$(uname)" == 'Darwin' ]]; then
	AWK=gawk
fi

$(grep -ve "^$" -ve "^#"  ~/.aws/credentials \
	| ${AWK} -F ' *= *' \
	'{
		if ($1 ~ /^\[/)
			section=$1;
		else if ($1 !~ /^$/ && section == "[default]" )
			print "export " toupper($1) "=" $2
	}')

$(aws ecr get-login --no-include-email)
docker-compose up
