#!/usr/bin/env bash
set -euo pipefail

declare -g input_file="$1" target
declare -ig counter=0
declare -ag target_array
target=$(tail -n1 "$input_file" | cut -d " " -f2)
readarray -d , -t target_array < <(cut -d " " -f2 <<<"$target")

# NOTES:
# 1 - length of output seems to be strictly increasing with size of a
# 2 - First digits of output seem to change fastest, last digits of output seem to change slowest

run_program() {
	./01.bash "$input_file" "$1"
}

find_length_range() {
	local -i min=0 max=0 incr=1
	local result

	while :; do
		result=$(run_program "$max")
		if ((${#result} < ${#target} + 1)); then
			((incr *= 2))
			((max += incr))
		else
			if ((incr == 2)); then ((max -= 1)) && break; fi
			((max -= incr))
			((incr /= 4))
		fi
	done

	incr=1
	while :; do
		result=$(run_program "$min")
		if ((${#result} < ${#target})); then
			((incr *= 2))
			((min += incr))
		else
			if ((incr == 2)); then break; fi
			# echo "Incr: $incr" >&2
			((min -= incr))
			((incr /= 4))
		fi
	done
	echo "$min" "$max"
}

find_digit_range() {
	local -i min="$1" max="$2" idx="$3" incr=1 seed=-1 step counter=0
	local -ai result
	((targ = target_array[idx], step = ($2 - $1) / 50000))

	for ((i = $1; i <= $2; i += step)); do
		readarray -d , -t result < <(run_program "$i")
		if ((result[idx] == targ)); then ((seed = i)) && break; fi
	done
	## Break if we can't find a seed
	if ((seed < 0)); then echo -1 && return 0; fi

	((min = (seed - step < $1) ? $1 : seed - step, incr = step / 2))
	while :; do
		readarray -d , -t result < <(run_program "$min")
		if ((result[idx] != targ)); then
			((min += incr))
		else
			if ((incr == 1)); then break; fi
			((incr /= 2))
			((min = (min - incr < $1) ? $1 : min - incr))
		fi
	done

	((max = seed, incr = step / 2))
	while :; do
		readarray -d , -t result < <(run_program "$max")
		if ((result[idx] != targ || max > $2)); then
			((incr = (incr >= 2) ? incr / 2 : 1))
			((max -= incr))
		else
			if ((incr == 1)); then break; fi
			((incr = 3 * incr / 2))
			((max += incr))
		fi
	done

	echo "$min" "$max"
}

read -r min max < <(find_length_range)
echo "$min" "$max"

for ((i = -1; i >= -5; i--)); do
	result=$(find_digit_range "$min" "$max" "$i")

	if [[ "$result" == "-1" ]]; then
		for ((j = min; j <= max; j++)); do
			program_result=$(run_program "$j")
			if ((j % 1000 == 0)); then
				echo "Min: $min, Max: $max, Current: $j"
				echo -e "\tResult: $program_result"
				echo -e "\tTarget: $target"
			fi
			if [[ "$program_result" == "$target" ]]; then
				echo "Success: $j"
				exit 0
			fi
		done
	else
		read -r new_min new_max <<<"$result"
		echo "New minimum: $new_min"
		echo "New maximum: $new_max"
		echo " Difference: $((new_max - new_min))"

		if ((new_min >= new_max)); then break; fi
		((min = new_min))
		((max = new_max))
	fi
done

# 35184372088832
# 246290604621824
