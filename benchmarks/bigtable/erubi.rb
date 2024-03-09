# BigTable benchmark implemented in erubi
#
require 'erubi'
require 'benchmark'

table = (1 .. 1000).map do |_| (1 .. 10) end

template = Erubi::Engine.new <<-EOF
<table>
  <% table.each do |row| %>
    <tr>
      <% row.each do |value| %>
        <td>
            <%= value %>
        </td>
      <% end %>
    </tr>
  <% end %>
</table>
EOF

number_runs = 100
start_time = Time.now.to_f
number_runs.times do
    eval(template.src)
end
end_time = Time.now.to_f

# start_time and end_time are both in seconds now
ms = (end_time - start_time) * 1000 / number_runs
puts "\"Erubi\", #{ms}"
