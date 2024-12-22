declare -a seeds
declare -A cache

mapfile -t seeds <"$1"

next_secret() {
	if [[ -v "cache[$1]" ]]; then
		echo "${cache[$1]}" && return 0
	fi

	local -i next_secret secret step1 step2
	secret="$1"

	((step1 = ((secret * 64) ^ secret) % 16777216))
	((step2 = ((step1 / 32) ^ step1) % 16777216))
	((next_secret = ((step2 * 2048) ^ step2) % 16777216))

	cache["$secret"]="$next_secret"
	echo "$next_secret"
}

predict_nth() {
	local -i secret="$1" times="$2" i
	for ((i = 1; i <= times; i++)); do
		read -r secret < <(next_secret "$secret")
	done
	echo "$secret"
}

declare -i count
for seed in "${seeds[@]}"; do
	predict_nth "$seed" 2000
	((count++))
	echo "Completed $count" >&2
done | paste -sd '+' | bc
