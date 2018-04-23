#! /bin/bash


for a in {1..11};
do
	for b in 1 2 3 4 5 6 7 8 9 10;
	do
		python SparCC/SparCC.py ${a}_$b.txt -i 10 --cor_file=${a}_$b-sparCC.out > sparcc.log;
		python SparCC/MakeBootstraps.py ${a}_$b.txt -n 100 -t permutation_#.txt -p example/pvals/
		for i in {0..99};
			do 	
			python SparCC/SparCC.py example/pvals/permutation_$i.txt -i 10 --cor_file=example/pvals/perm_cor_$i.txt
			done			
		python SparCC/PseudoPvals.py ${a}_$b-sparCC.out example/pvals/perm_cor_#.txt 100 -o ${a}_$b-pvals.txt -t "two_sided"
		echo "${a} $b done!";
	done
done
