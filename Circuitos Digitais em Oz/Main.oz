functor

import
   Browser(browse:Browse) 
   CircuitosFunctor at 'Functor_Circuitos.ozf'
define
   A = 0|_
   B = 1|_
   Bin = 1|_ S Bout

   D = 0|_
   E = 0|_
   F = 0|_
   G = 0|_ O
   
   H=0|_
   I=1|_ S1 S2 S3

   %Paridade par
   {Browse 'Circuito de ParidadePar: '}
   {CircuitosFunctor.paridadePar D E F G O}
   {Browse entrada(D E F G)}
   {Browse saida(O)}

   %Comparador
   {Browse 'Circuito de Comparador: '}
   {CircuitosFunctor.comparador H I S1 S2 S3}
   {Browse entrada('B:' H 'A:' I)} 
   {Browse saida('A > B:' S1)}
   {Browse saida('A = B:' S2)}
   {Browse saida('A < B:' S3)}


   %Subtractor
   {Browse 'Circuito de Subtracao:'}
   {Browse 'diferença (A-B), utilizando xorG:'}
   {Browse entrada('A:' A 'B:' B 'Bin:' Bin)}
   {CircuitosFunctor.subtractor A B Bin S Bout}
   {Browse saida('S:' S)}
   {Browse 'Usando orG, andG e notG, empresta um :'}
   {Browse saida(Bout)}

end

