functor

import 
    PortasFunctor at 'Functor_Portas.ozf' %nome do arquivo
export
    paridadePar:ParidadePar
    comparador:Comparador
    subtractor:Subtractor
define
    
    proc {ParidadePar A B C D ?O}
        K L M N P Q
        in
        K = {PortasFunctor.xnorG D C}
        L = {PortasFunctor.xnorG B A}
        M = {PortasFunctor.xorG D C}
        N = {PortasFunctor.xorG B A}
        P = {PortasFunctor.andG K L}
        Q = {PortasFunctor.andG M N}
        O = {PortasFunctor.orG P Q}
    end
  
    proc {Comparador A B ?S1 ?S2 ?S3}
        S1 = {PortasFunctor.andG {PortasFunctor.notG A} B}
        S3 = {PortasFunctor.andG A {PortasFunctor.notG B}}
        S2 = {PortasFunctor.norG S1 S3}
    end
   
    proc {Subtractor A B Bin ?S ?Bout }
        AB    
        in
        AB = {PortasFunctor.xorG A B}
        S= {PortasFunctor.xorG AB Bin}
        Bout = {PortasFunctor.orG {PortasFunctor.andG {PortasFunctor.notG AB} Bin} {PortasFunctor.andG {PortasFunctor.notG A} B}}
    end

end
