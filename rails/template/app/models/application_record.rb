class ApplicationRecord < ActiveRecord::Base
  primary_abstract_class

  def format_for_inspect(name, value)
    if value.nil?
      value.inspect
    else
      inspected_value = if value.is_a?(String) && value.length > 500
                          "#{value[0, 500]}...".inspect
                        elsif value.is_a?(Date) || value.is_a?(Time)
                          %("#{value.to_fs(:inspect)}")
                        else
                          value.inspect
                        end

      inspection_filter.filter_param(name, inspected_value)
    end
  end
end
