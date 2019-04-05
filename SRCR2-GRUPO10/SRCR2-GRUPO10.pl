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
