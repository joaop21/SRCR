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

utente(1,joao,31,guimaraes).
utente(2,manuel,57,viana).
utente(3,armando,26,porto).
utente(4,ricardo,23,famalicao).
utente(5,maria,40,braga).
utente(6,miguel,26,guimaraes).
utente(7,ana,14,braga).
utente(8,andre,26,amares).
utente(9,henrique,14,fafe).
utente(10,diogo,14,braga).

utente( IU,N,A,CI ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado serviço: IdServ,Descrição,Instituição,Cidade -> {V,F}

servico(1,cardiologia,hospitaldaluz,guimaraes).
servico(2,pediatria,hospitalbraga,braga).
servico(3,cirurgia,hospitalbraga,braga).
servico(4,neurologia,hsj,porto).
servico(5,ginecologia,hospitalbraga,braga).
servico(6,psiquiatria,hsog,guimaraes).
servico(7,oftamologia,hsog,guimaraes).

servico( IS,D,I,CI ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado consul: Data,IdUt,IdServ,Custo-> {V,F}

consul(01-02-2019, 1, 6, 25).
consul(13-02-2019, 3, 4, 30).
consul(13-02-2019, 5, 5, 35).
consul(14-02-2019, 2, 7, 9).
consul(20-02-2019, 7, 2, 20).
consul(23-02-2019, 7, 8, 5).
consul(23-02-2019, 5, 5, 24).
consul(25-02-2019, 6, 7, 40).
consul(29-02-2019, 7, 2, 65).
consul(04-03-2019, 9, 2, 95).
consul(07-03-2019, 1, 1, 10).

consul( DA,IU,IS,C ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cidade: IdUt,Cidade-> {V,F}

utente_cidade(IU,C).
