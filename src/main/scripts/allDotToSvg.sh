for i in *.dot;
do dot -Tsvg -o${i%.*}.svg $i;
done
