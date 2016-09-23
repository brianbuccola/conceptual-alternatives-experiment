#!/bin/bash

generateitem_script="./generateitem.py"
items_dir="../chunk_includes"
number_of_items_per_condition=6
number_of_practice_items=4

Q1s=("Some" "NotAll" "AtLeast3" "AtMost3" "AtLeast4" "AtMost4")
Q2s=("All" "No" "SBNA" "Exactly3" "Exactly4")
acceptable_rules=("Some All" "Some SBNA" "NotAll No" "NotAll SBNA" "AtLeast3 Exactly3" "AtMost3 Exactly3" "AtLeast4 Exactly4" "AtMost4 Exactly4")
conditions=("true" "false" "target")

# Make items_dir.
mkdir -p "${items_dir}"

# Generate some practice items (true/false conditions).
"${generateitem_script}" "Some" "All" "true" > "${items_dir}/practice1.html"
"${generateitem_script}" "Some" "All" "false" > "${items_dir}/practice2.html"
"${generateitem_script}" "NotAll" "SBNA" "true" > "${items_dir}/practice3.html"
"${generateitem_script}" "Some" "SBNA" "false" > "${items_dir}/practice4.html"

i=1
for Q1 in "${Q1s[@]}"; do
  for Q2 in "${Q2s[@]}"; do
    for rule in "${acceptable_rules[@]}"; do
      if [[ "${Q1} ${Q2}" == "${rule}" ]]; then
        for condition in "${conditions[@]}"; do
          j=0
          while (( j++ < ${number_of_items_per_condition} )); do
            "${generateitem_script}" "${Q1}" "${Q2}" "${condition}" > "${items_dir}/item$i.html"
            let i+=1
          done
        done
      fi
    done
  done
done
