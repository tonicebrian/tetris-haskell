guard :shell, all_on_start: true do
  watch /src.+\.l?hs$/ do |m|
    puts "\n\n\nCompiling..."
    `cabal build && echo "Compiled!"`
    end
end
