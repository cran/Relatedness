#include <R.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/* Test de l'EM */




void BoucleEMacc(double *ProbCond , int *NbConfIBD , int *NbSnp , double *prec , double *delta , double *CondVrais){

	/* ProbCond est une matrice de taille NbSnp lignes et NbConfIBD colonnes */
	/* prec est la précision demandée à l'EM */
	/* delta est un vecteur de taille NbConfIBD (ici 15) et c'est le résultat voulu en sortie */
	/* CondVrais est une matrice de taille NbSnp lignes est NbConfIBD colonnes */

	/* Les compteur pour les lignes et les colonnes dans les boucles */
	int CptCol , CptLig;
	
	int it=0;
	
	double* RowSumCV = (double*)malloc(*NbSnp * sizeof(double));
	double* DiffDelta = (double*)calloc(*NbConfIBD, sizeof(double));
	double* DiffDeltaOld = (double*)calloc(*NbConfIBD, sizeof(double));
	double* DiffDeltaacc = (double*)calloc(*NbConfIBD, sizeof(double));
	double* deltaold = (double*)calloc(*NbConfIBD, sizeof(double));
	double* deltaanc = (double*)calloc(*NbConfIBD, sizeof(double));
	double* deltaacc = (double*)calloc(*NbConfIBD, sizeof(double));
	
	double* r = (double*)calloc(*NbConfIBD, sizeof(double));
	double* v = (double*)calloc(*NbConfIBD, sizeof(double));

	double sommer2;
	double sommev2;
	double Alpha;
	double Crit;
	double Tamp;

	bool sommeNeg;

	/* RowSumCV est le vecteur de la somme sur les lignes de CondVrai (taille NbSnp) */
	/* Tau était une matrice de taille NbSnp lignes et NbConfIBD colonnes (n'est plus necessaire) */
	/* deltaold est un vecteur enregistrant le delta précédent (taille NbConfIBD) */
	/* Crit est le critère d'arrêt de la boucle (à comparer à la précision) */
	/* DiffDelta est la différence entre delta et deltaold après chaque itération */
	/* Tamp est une variable temporaire permettant les réécriture (toujours mise à 0 après chaque modification) */

	Crit=1;
	Tamp = 0;
	

	int tmplig; // variable permettant d'economiser du calcul repetitif dans les boucles imbriquées.
	
	while (Crit > *prec)
	{
		/* 1 - On enregistre le delta au début de l'itération dans deltaold */
		for (CptCol=0 ; CptCol < *NbConfIBD ; CptCol++)
		{
			deltaanc[CptCol] = deltaold[CptCol];
			deltaold[CptCol] = delta[CptCol];
			delta[CptCol] = 0; // necessaire pour l'etape 3
		}
		Tamp=0;

		/* 2 - On écrit la matrice CondVrai comme le produit de la matrice ProbCond et du vecteur deltaold 
		       et on effectue la somme sur les lignes (stocké dans RowSumCV)
		       NB: on a fusionné la double boucle sur CondVrais et celle sur RowSumCV en une seule */
		for (CptLig=0 ; CptLig < *NbSnp ; CptLig++)
		{
			tmplig = CptLig * *NbConfIBD;
			for (CptCol=0 ; CptCol < *NbConfIBD ; CptCol++)
			{
				CondVrais[tmplig + CptCol] = ProbCond[tmplig + CptCol] * deltaold[CptCol];
				//~ CondVrais[CptLig * NbConfIBD + CptCol] = ProbCond[CptLig * NbConfIBD + CptCol] * deltaold[CptCol];
				Tamp += CondVrais[tmplig + CptCol]; ////
			}
			RowSumCV[CptLig] = Tamp;
			Tamp = 0;
		}

		/* 3 - On normalise CondVrais (somme sur les lignes = 1)
		       puis on somme les colonnes (le tout est directement stocké dans le vecteur delta)
			   NB: on economise les deux double-boucles sur Tau (la matrice Tau n'est plus necessaire).
			       la normalisation de delta par le nombre de Snp est faite à l'étape 4 */
		for (CptLig=0 ; CptLig < *NbSnp ; CptLig++)
		{
			tmplig = CptLig * *NbConfIBD;
			for (CptCol=0 ; CptCol < *NbConfIBD ; CptCol++)
			{
				//~ Tau[tmplig + CptCol] = CondVrais[tmplig + CptCol] / RowSumCV[CptLig];
				delta[CptCol] += CondVrais[tmplig + CptCol] / RowSumCV[CptLig];
			}
		}		
		/* 4 - On normalise delta par le nombre de Snp 
		       puis On calcule la différence de delta et deltaold */
		for (CptCol=0 ; CptCol < *NbConfIBD ; CptCol++)
		{
			delta[CptCol] /= (double)*NbSnp;
			//~ fprintf(stderr,"%.20lf ",delta[CptCol]);
			DiffDeltaOld[CptCol]=DiffDelta[CptCol];
			r[CptCol]=DiffDelta[CptCol];
			sommer2 += r[CptCol]*r[CptCol];
 			DiffDelta[CptCol]=delta[CptCol]-deltaold[CptCol];
			v[CptCol]=DiffDelta[CptCol] - DiffDeltaOld[CptCol];
			sommev2 += v[CptCol]*v[CptCol];
		}
		it ++;
		/* 4bis - On utilise Ravi */
		if (it==2)
			{
                        Alpha = -sqrt(sommer2/sommev2);
                        //double Alpha = arma::conv_to<double>::from(- sqrt(sum(pow(r,2))/sum(pow(v,2))));
                        if (Alpha > -1)
			{
                        	Alpha = -1;
                        }	
			
			sommeNeg=FALSE;
				
			for (CptCol=0 ; CptCol < *NbConfIBD ; CptCol++)
			{
				deltaacc[CptCol] = deltaanc[CptCol] - 2*Alpha*r[CptCol] + (Alpha*Alpha) * v[CptCol];
				DiffDeltaacc[CptCol]=delta[CptCol]-deltaanc[CptCol];
				sommeNeg = (sommeNeg)||(delta[CptCol]<0);
			}
                        //SigmaNew = SigmaAnc - 2*Alpha*r + pow(Alpha,2)*v;

                        if (!sommeNeg)
			{
				for (CptCol=0 ; CptCol < *NbConfIBD ; CptCol++)
				{
	                        	delta[CptCol]=deltaacc[CptCol];
					DiffDelta[CptCol]=DiffDeltaacc[CptCol];
				}
                        }
                          
                          
                        //crit = max(abs(SigmaNew-SigmaAnc));
                        it = 0;
		}

		sommer2=0;
		sommev2=0;

		/* 5 - Le critère devient la norme euclidienne de la différence */
		for (CptCol=0 ; CptCol < *NbConfIBD ; CptCol++)
		{
			Tamp += DiffDelta[CptCol] * DiffDelta[CptCol];
		}
		Crit = sqrt(Tamp);
	}
	free(RowSumCV);
	free(DiffDelta);
	free(deltaold);
	free(deltaanc);
	free(deltaacc);
	free(DiffDeltaOld);
	free(DiffDeltaacc);
	free(v);
	free(r);
}

