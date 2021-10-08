# Creating a full-stack clojure app

# Create a project

```shell
mkdir atemoia
cd atemoia
echo {} > deps.edn
mkdir dev src
```

`deps.edn`

```clojure
{:paths   ["src"]
 :deps    {com.github.seancorfield/next.jdbc {:mvn/version "1.2.674"}
           hiccup/hiccup                     {:mvn/version "2.0.0-alpha2"}
           io.pedestal/pedestal.jetty        {:mvn/version "0.5.9"}
           io.pedestal/pedestal.service      {:mvn/version "0.5.9"}
           org.clojure/clojure               {:mvn/version "1.10.3"}
           org.postgresql/postgresql         {:mvn/version "42.2.23"}}
 :aliases {:dev {:extra-paths ["dev"]
                 :extra-deps  {io.github.clojure/tools.build {:git/url "https://github.com/clojure/tools.build.git"
                                                              :sha     "1e7c019730dc6f9e38793170c8801c5950516b60"}
                               reagent/reagent               {:mvn/version "1.1.0"}
                               com.google.guava/guava        {:mvn/version "30.1.1-jre"}
                               thheller/shadow-cljs          {:mvn/version "2.15.2"}}}}}
```

# Create a backend

```shell
mkdir src/atemoia
echo '(ns atemoia.server)' > src/atemoia/server.clj
```

`server.clj`
```clojure
```