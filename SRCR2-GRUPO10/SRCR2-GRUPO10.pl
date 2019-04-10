%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica estendida
% Representacao de conhecimento imperfeito

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento para caracterizar um universo de discurso na área da prestação de cuidados de saúde.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- dynamic '-'/1.
:- op( 900,xfy,'::' ).
:- dynamic utente/5.
:- dynamic servico/4.
:- dynamic consulta/5.
:- dynamic medico/4.
:- dynamic seguro/3.


%--------------------------REPRESENTACAO CONHECIMENTO POSITIVO--------------------------%

% carregaFactos('GRUPO10_FACTOS.txt').




%--------------------------REPRESENTACAO CONHECIMENTO NEGATIVO--------------------------%

% Extensao do predicado utente: IdUt,Nome,Idade,Cidade,Seguro-> {V,F,D}
-utente(IU,N,I,C,IdS) :-
    nao(utente(IU,N,I,C,IdS)),
    nao(excecao(utente(IU,N,I,C,IdS))).

% Extensao do predicado serviço: IdServ,Descrição,Instituição,Cidade -> {V,F,D}
-servico(IS,D,I,C) :-
    nao(servico(IS,D,I,C)),
    nao(excecao(servico(IS,D,I,C))).

% Extensao do predicado consulta: Data,IdUt,IdServ,Custo,IdMed-> {V,F,D}
-consulta(DA,IU,IS,C,IM) :-
    nao(consulta(DA,IU,IS,C,IM)),
    nao(excecao(consulta(DA,IU,IS,C,IM))).

% Extensao do predicado medico: IdMed, Nome, Idade, IdServ -> {V,F,D}
-medico(IM,N,I,IS) :-
    nao(medico(IM,N,I,IS)),
    nao(excecao(medico(IM,N,I,IS))).

% Extensao do predicado seguro: IdSeg,Descrição,Taxa -> {V,F,D}
-seguro(IdSeg,D,T) :-
    nao(seguro(IdSeg,D,T)),
    nao(excecao(seguro(IdSeg,D,T))).


%--------------------------REPRESENTACAO CONHECIMENTO IMPERFEITO INCERTO--------------------------%

% Não sabemos qual a Instituição que prestou o serviço 9 correspondente a Fisiatria, apenas sabemos que foi realizada em Braga 
servico(9,fisiatria,xpto021,braga).
excecao( servico(IDS,D,I,C) ) :-
         servico(IDS,D,xpto021,C).

% Não sabemos qual é a cidade onde reside o Utente 11 chamado Alfredo, com 86 anos e portador do seguro 2
utente(11,alfredo,86,xpto022,2).
excecao( utente(IU,N,I,C,IdS) ) :-
         utente(IU,N,I,xpto022,IdS).

% Não sabemos o serviço que o médico 8, chamado António e com 37 anos realiza
medico(8,antonio,37,xpto023).
excecao( medico(IM,N,I,S) ) :-
         medico(IM,N,I,xpto023).


%--------------------------REPRESENTACAO CONHECIMENTO IMPERFEITO IMPRECISO--------------------------%

% Não sabemos se o serviço 10 de pneumologia foi realizado no Hospital de Braga ou no Hospital Senhora da Oliveira em Guimarães
excecao( servico(10,pneumologia,hospitalbraga,braga) ).
excecao( servico(10,pneumologia,hsog,guimaraes) ).

% Não qual a taxa do Seguro 3 Allianz, apenas sabemos que está entre 0.2 e 0.25
excecao( seguro(3,allianz,X) ) :-
         X >= 0.2,
         X =< 0.25.

%--------------------------REPRESENTACAO CONHECIMENTO IMPERFEITO INTERDITO--------------------------%

% Nunca será possivel saber o custo da consulta realizada no dia 10 de Abril de 2019 pelo médico 1 que frequenta o serviço 7.
% Esta consulta foi requisitada pelo utente 2
consulta(data(10,4,2019),2,7,xpto024,1).
excecao( consulta(DA,IU,IS,C,IM) ) :-
         consulta(DA,IU,IS,xpto024,IM).
nulo(xpto024).
  
+consulta( DA,IU,IS,C,IM ) :: (solucoes((CS), (consulta(data(10,4,2019),2,7,CS,1), nao(nulo(CS))), S),
                    comprimento( S,N ), N == 0
                    ).


%--------------------------EVOLUCAO E REGRESSAO DO CONHECIMENTO--------------------------%

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

evolucao(Termo) :-
    solucoes(Invariante,+Termo::Invariante,Lista),
    insercao(Termo),
    teste(Lista).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a regressão do conhecimento

regressao(Termo) :-
	  Termo,
	  solucoes(Invariante,-Termo::Invariante,Lista),
	  remover(Termo),
    teste(Lista).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite encontrar as provas

solucoes(F,Q,S) :-
    Q, assert(tmp(F)), fail.
solucoes(F,Q,S) :-
    construir(S,[]).

construir(S1,S2) :-
    retract(tmp(X)), !,
    construir(S1, [X|S2]).
construir(S,S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a inserção do conhecimento
insercao(Termo) :-
    assert(Termo).
insercao(Termo) :-
    retract(Termo), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a remoção do conhecimento
remover(Termo) :-
    retract(Termo).
remover(Termo) :-
    assert(Termo), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que realiza o teste do conhecimento
teste([]).
teste([R|LR]) :-
    R,
    teste(LR).




%--------------------------SISTEMA DE INFERENCIA--------------------------%

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado si: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }

si( Questao,verdadeiro ) :-
    Questao.
si( Questao,falso ) :-
    -Questao.
si( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------GUARDAR EM FICHEIRO--------------------------%

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite guardar a base de conhecimento num ficheiro
% guardaFactos: Ficheiro -> {V,F}

guardaFactos(Ficheiro) :-
    tell(Ficheiro),
    listing,
    told.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite carregar a base de conhecimento dum ficheiro
% carregaFactos: Ficheiro -> {V,F}

carregaFactos(Ficheiro) :-
    seeing(InputAtual),
    see(Ficheiro),
    repeat,
    read(Termo),
    (Termo == end_of_file -> true ;
    assert(Termo),fail),
    seen,
    see(InputAtual).
