local setup, comment = pcall(require, "Comment") -- install comment into this variable 'comment'
if not setup then
  return
end

comment.setup()
