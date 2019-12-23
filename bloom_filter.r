library(digest)
n=10
m=100
nh=4
l=rep(0,m)
user = c("a","b","c","d","e","f","g","h","i","j")
notuser = c("z","y","x","w","v","u","t","s","q","p")

get_hash = function(item, seed)
{
  hex_str = digest(object=item, algo="murmur32", serialize=F, seed=seed)
  hex = paste('0x',hex_str, sep="")
  return(as.numeric(hex)%%m)
}

add = function(item)
{
  for(i in 1:nh)
  {
    hash_digest = get_hash(item,i)
    hash_digest=hash_digest+1
    l[hash_digest]<<-1
  }
}
check = function(item)
{
  for(i in 1:nh)
  {
    hash_digest = get_hash(item,i)
    hash_digest=hash_digest+1
    if (l[hash_digest]==0)
    {
      return (FALSE)
    }
    return (TRUE)
  }
}

for (i in 1:n)
{
  add(user[i])
}

testset = c(user[1:5], notuser)
testset

for (i in 1:length(testset))
{
  if(check(testset[i]))
  {
    if (testset[i] %in% notuser)
    {
      cat(testset[i]," is a false positive\n")
    }
    else
    {
      cat(testset[i]," is probably a user\n")
    }
  }
  else
  {
    cat(testset[i], "is definitely not a user!\n")
  }
}
l