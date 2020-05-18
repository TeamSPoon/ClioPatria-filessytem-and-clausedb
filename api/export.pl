/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(api_export, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_schema)).
:- use_module(rdfql(rdf_io)).
:- use_module(rdfql(rdf_turtle_io)).
:- use_module(user(user_db)).

:- http_handler(api(export_graph),         export_graph,  []).
:- http_handler(api(export_graph_schema),  export_graph_schema,  []).

/** <module> Export data from the server

*/

%!  export_graph(+Request)
%
%   Export a named graph in a   given  serialization. Whether or not
%   exporting of a named graph is  defined by authorized/1 using the
%   term:
%
%           * read(default, download(Graph))

export_graph(Request) :-
	http_parameters(Request,
			[ graph(Graph),
			  format(Format),
			  mimetype(Mime)
			],
			[ attribute_declarations(http_param)
			]
		       ),
	authorized(read(default, download(Graph))),
	send_graph(Graph, Format, Mime).

send_graph(Graph, Format, default) :-
    !,
	default_mime_type(Format, MimeType),
	send_graph(Graph, Format, MimeType).
send_graph(Graph, Format, MimeType) :-
    !,
	format('Transfer-Encoding: chunked~n'),
	format('Content-type: ~w; charset=UTF8~n~n', [MimeType]),
	send_graph(Graph, Format).

send_graph(Graph, turtle) :-
    !,
	rdf_save_turtle(stream(current_output),
                    [ graph(Graph),
                      expand(triple_in_graph(graph(Graph))),
                      base(Graph)
			]).
send_graph(Graph, canonical_turtle) :-
    !,
	rdf_save_canonical_turtle(stream(current_output), [graph(Graph)]).
send_graph(Graph, rdfxml) :-
    !,
	rdf_save(stream(current_output), [graph(Graph)]).



% called from send_graph/2.
:- thread_local triple_in_graph_local/5.
:- public triple_in_graph/5.
:- export(triple_in_graph/5).


triple_in_graph(RDF,S,P,O,G) :- predicate_property(M:triple_in_graph_local(_,_,_,_,_),number_of_clauses(N)),N>0,!, M:triple_in_graph_local(RDF, S,P,O,G).
triple_in_graph(RDF,S,P,O,G):- triple_in_graph(RDF,List),!,(member(rdf(S,P,O,G),List);member(rdf(S,P,O),List)).


tst:- rdf_save_turtle(stream(current_output), [graph(mud)]).



triple_in_graph(graph(G),List) :- nonvar(G),!,findall(rdf(S,P,O,G),rdf(S,P,O,G),List),!.
triple_in_graph(G,List) :- nonvar(G),rdf_graph(G),!,findall(rdf(S,P,O,G),rdf(S,P,O,G),List),!.
triple_in_graph(List,List) :- member(rdf(_,_,_,_),List),!.
triple_in_graph(List,List) :- member(rdf(_,_,_),List),!.
triple_in_graph(rdf(S,P,O),List) :- findall(rdf(S,P,O,G),rdf(S,P,O,G:_),List),!.
triple_in_graph(rdf(S,P,O,Gf),List) :- ((Gf=GN:N)-> Gf=GN:N ; (Gf=GN,N=_)),  !, findall(rdf(S,P,O,GN),rdf(S,P,O,GN:N),List),!.
triple_in_graph(triples_with(Triples,ValuesNeeded),List):- triple_in_graph(Triples,InitList),!,triples_with_values(InitList,ValuesNeeded,List).


triples_with_values(InitList,ValuesNeeded,TriplesKeptO):-
   triples_with_values_list(InitList,ValuesNeeded,[],NewValuesList,TriplesKept,TriplesUnkept),
   ((NewValuesList == []) -> TriplesKeptO = TriplesKept;
   (triples_with_values(TriplesUnkept,NewValuesList,NewTriplesKept),append(TriplesKept,NewTriplesKept,TriplesKeptO))).


add_to_list(ValuesNeeded,P,NewValuesIn,NewValuesOut):- 
    member(P,ValuesNeeded) -> NewValuesIn=NewValuesOut;
     member(P,NewValuesIn) -> NewValuesIn=NewValuesOut;
      [P|NewValuesIn]=NewValuesOut.

triples_with_values_list([],_,NewValuesInOut,NewValuesInOut,[],[]):-!.

triples_with_values_list([rdf(S,P,O,G)|InitList],ValuesNeeded,NewValuesIn,NewValuesOutO,[rdf(S,P,O,G)|TriplesKept],TriplesUnkept):-
           member(S,ValuesNeeded), 
            % add_to_list(ValuesNeeded,P,NewValuesIn,NewValuesMid),
            % add_to_list(ValuesNeeded,O,NewValuesMid,NewValuesOut),
        triples_with_values_list(InitList,ValuesNeeded,NewValuesIn,NewValuesOutO,TriplesKept,TriplesUnkept).

triples_with_values_list([rdf(S,P,O,G)|InitList],ValuesNeeded,NewValuesIn,NewValuesOutO,[rdf(S,P,O,G)|TriplesKept],TriplesUnkept):-
      member(P,ValuesNeeded), add_to_list(ValuesNeeded,S,NewValuesIn,NewValuesOut), % add_to_list(ValuesNeeded,O,NewValuesMid,NewValuesOut),
   triples_with_values_list(InitList,ValuesNeeded,NewValuesOut,NewValuesOutO,TriplesKept,TriplesUnkept).

triples_with_values_list([rdf(S,P,O,G)|InitList],ValuesNeeded,NewValuesIn,NewValuesOutO,[rdf(S,P,O,G)|TriplesKept],TriplesUnkept):-
      member(O,ValuesNeeded), add_to_list(ValuesNeeded,S,NewValuesIn,NewValuesMid), % add_to_list(ValuesNeeded,P,NewValuesMid,NewValuesOut),
   triples_with_values_list(InitList,ValuesNeeded,NewValuesMid,NewValuesOutO,TriplesKept,TriplesUnkept).

triples_with_values_list([rdf(S,P,O,G)|InitList],ValuesNeeded,NewValuesIn,NewValuesOut,TriplesKept,[rdf(S,P,O,G)|TriplesUnkept]):-
      triples_with_values_list(InitList,ValuesNeeded,NewValuesIn,NewValuesOut,TriplesKept,TriplesUnkept).
        


default_mime_type(turtle, text/turtle).
default_mime_type(canonical_turtle, text/turtle).
default_mime_type(rdfxml, application/'rdf+xml').

%!  export_graph_schema(+Request)
%
%   HTTP handler that computes the schema from the actual data in a
%   graph.
%
%   @see The computation is implemented by rdf_graph_schema/2.

export_graph_schema(Request) :-
    http_parameters(Request,
                    [ graph(Graph),
                      format(Format),
                      mimetype(Mime)
                    ],
                    [ attribute_declarations(http_param)
                    ]
                   ),
    authorized(read(default, download(Graph))),
    rdf_graph_schema(Graph, Triples),
    (   Mime == default
    ->  default_mime_type(Format, MimeType)
    ;   MimeType = Mime
    ),
    write_graph(Triples,
                [ serialization(Format),
                  mimetype(MimeType)
                ]).


%!  http_param(?Name, ?Attributes).

http_param(graph,
           [ description('Name of the graph')]).
http_param(format,
           [ oneof([turtle,
                    canonical_turtle,
                    rdfxml
                   ]),
             default(turtle),
             description('Output serialization')
           ]).
http_param(mimetype,
           [ default(default),
             description('MIME-type to use. If "default", it depends on format')
           ]).



