%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento para caracterizar um universo de discurso na área da prestação de cuidados de saúde.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IdUt,Nome,Idade,Cidade-> {V,F}

utente( IU,N,A,C) :-
	


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado serviço: IdServ,Descrição,Instituição,Cidade -> {V,F}

servico( IS,D,I,C) :-


% Extensao do predicado consulta: Data,IdUt,IdServ,Custo-> {V,F}

consulta(DA,IU,IS,C) :-


% Extensao do predicado cidade: IdUt,Cidade-> {V,F}

utente_cidade(IU,C) :-
	
