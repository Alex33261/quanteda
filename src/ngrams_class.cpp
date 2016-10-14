#include <Rcpp.h>
#include <unordered_map>
#include <numeric>
#include "dev.hpp"
#include "quanteda.hpp"

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace std;
using namespace quanteda;

typedef std::vector<unsigned int> Ngram;
typedef std::vector<unsigned int> Ngrams;

namespace std {
  template <>
  // Custom hash function for Ngram objects
  struct hash<Ngram> {
    std::size_t operator()(const Ngram &vec) const {
      unsigned int seed = std::accumulate(vec.begin(), vec.end(), 0);
      return std::hash<unsigned int>()(seed);
    }
  };
}


class ngramMaker {
    
    public:
    ngramMaker(){}
      
    int ngram_id(Ngram ngram){
      
        // Add new ID without multiple access
        unsigned int &id_ngram = map_ngram[ngram];
        if(id_ngram){
          //Rcout << "Old " << id_ngram << ": ";
          //dev::print_ngram_hashed(ngram);
          return id_ngram;
        }
        id_ngram = map_ngram.size();
        //Rcout << "New " << id_ngram << ": ";
        //dev::print_ngram_hashed(ngram);
        return id_ngram;
    }
    
    void skip(NumericVector &tokens,
                     unsigned int start,
                     unsigned int n, 
                     NumericVector skips,
                     Ngram ngram,
                     Ngrams &ngrams,
                     int pos_tokens, int &pos_ngrams
    ){
      
        ngram[pos_tokens] = tokens[start];
        pos_tokens++;
        
        //Rcout << "Token " << tokens[start] << "\n";
        if(pos_tokens < n){
            for (int j = 0; j < skips.size(); j++){
              int next = start + skips[j];
              if(next < 0 || tokens.size() - 1 < next) break;
              //Rcout << "Join " << ngram << " at " << pos_tokens << " " << next << "\n";
              skip(tokens, next, n, skips, ngram, ngrams, pos_tokens, pos_ngrams);
            }
        }else{
            //Rcout << "Add " << ngram << " at " << pos_ngrams << "/" << ngrams.size() << "\n";
            ngrams[pos_ngrams] = ngram_id(ngram);
            pos_tokens = 0;
            pos_ngrams++;
        }
    }
    
    
    Ngrams ngram(NumericVector tokens, NumericVector ns, NumericVector skips) {
      
        int pos_tokens = 0; // Position in tokens
        int pos_ngrams = 0; // Position in ngrams
        
        // Pre-allocate memory
        int size_reserve = 0;
        for (int k = 0; k < ns.size(); k++) {
          size_reserve += std::pow(skips.size(), ns[k]) * tokens.size();
        }
        Ngrams ngrams(size_reserve);
        
        // Generate skipgrams recursively
        for (int k = 0; k < ns.size(); k++) {
          int n = ns[k];
          Ngram ngram(n);
          for (int start = 0; start < tokens.size() - (n - 1); start++) {
            skip(tokens, start, n, skips, ngram, ngrams, pos_tokens, pos_ngrams); // Get ngrams as reference
          }
        }
        ngrams.resize(pos_ngrams - 1);
        return ngrams;
    }
    

    List ngram_vector(NumericVector tokens, NumericVector ns, NumericVector skips){
      
        // Register both ngram (key) and unigram (value) IDs in a hash table
        Ngrams ngrams = ngram(tokens, ns, skips);
        
        return Rcpp::List::create(Rcpp::Named("ngram") = ngrams,
                                  Rcpp::Named("id_unigram") = vocaburary());
    }
    
      
    XPtr<Tokens> ngram_list_ptr(List texts, NumericVector ns, NumericVector skips) {
      
      // Itterate over documents
      List texts_ngram(texts.size());
      for (int h = 0; h < texts.size(); h++){
        Rcpp::checkUserInterrupt(); // allow user to stop
        texts_ngram[h] = ngram(texts[h], ns, skips);
      }
      
      // Return pointer to Tokens object
      Tokens* tokens = new Tokens(texts_ngram, vocaburary());
      return Rcpp::XPtr<Tokens>(tokens);
    }
    
    List ngram_list(List texts, NumericVector ns, NumericVector skips) {
        
        // Itterate over documents
        List texts_ngram(texts.size());
        for (int h = 0; h < texts.size(); h++){
            Rcpp::checkUserInterrupt(); // allow user to stop
            texts_ngram[h] = ngram(texts[h], ns, skips);
        }
        return Rcpp::List::create(Rcpp::Named("text") = texts_ngram,
                                  Rcpp::Named("id_unigram") = vocaburary());
    }
    
      
    List vocaburary(){
        
        // Separate key and values of unordered_map
        List ids_unigram(map_ngram.size());
        for (std::pair<Ngram, unsigned int> iter : map_ngram){
          //Rcout << "ID " << to_string(iter.second) << ": ";
          //print_ngram_hashed(iter.first);
          ids_unigram[iter.second - 1] = iter.first;
        }  
        return ids_unigram;
    }
    
    private:
    // Register both ngram (key) and unigram (value) IDs in a hash table
    std::unordered_map<Ngram, unsigned int> map_ngram; 
};

// Expose C++ class to R
RCPP_MODULE(ngram_module) {
  class_<ngramMaker>("ngramMaker")
  .constructor()
  .method("generate_vector", &ngramMaker::ngram_vector)
  .method("generate_list", &ngramMaker::ngram_list)
  .method("generate_list_ptr", &ngramMaker::ngram_list_ptr)
  ;
}


/*** R

nm <- new(ngramMaker)

chars <- rep(head(letters), 2)
types <- unique(chars)
chars_hashed <- match(chars, types)
res <- nm$generate_vector(chars_hashed, 3, 1)

ngram <- res$ngram
ngram_ids <- res$id_ngram
vocaburary <- sapply(res$id_unigram, function(x, y, z) paste(y[x], collapse=z) , types, '-')
vocaburary[ngram]

tokens <- tokenize(c('a b c d e', 'c d e f g'))
tokens_hashed <- hashTokens(tokens)
res <- nm$generate_list(tokens_hashed, 2, 0:1)
ptr <- nm$generate_list_ptr(tokens_hashed, 2, 0:1)


*/

