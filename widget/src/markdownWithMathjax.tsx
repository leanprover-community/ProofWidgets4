import remarkMath from 'remark-math'
import Markdown from 'react-markdown'

// @ts-ignore
import rehypeMathjax from 'rehype-mathjax'

export default ({markdown} : {markdown : string}) => {
  return (<Markdown remarkPlugins={[remarkMath]} rehypePlugins={[rehypeMathjax]}>
    {markdown}
  </Markdown>);
}
