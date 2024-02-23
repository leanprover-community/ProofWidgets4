import { RpcSessionAtPos } from '@leanprover/infoview'

type RequestId = string
type CheckRequestResponse<S> =
  'running' | { done: { result: S } }

/** An object for passing a function by reference. */
export type Fn = {
  fn: () => void
}

/** Returns a promise of a response to this request
 * as well as a function you can call to cancel the request.
 * It is request-specific what a cancelled request resolves with:
 * it could ignore the cancellation altogether,
 * return a null result,
 * or throw an error. */
export function callCancellable<T, S>(rs: RpcSessionAtPos, name: string, params: T):
    [Promise<S>, Fn] {
  const cancelFn = {
    fn: () => {}
  }
  const promise = new Promise<S>(async (resolve, reject) => {
    const id: RequestId = await rs.call(name, params)
    const i = window.setInterval(async () => {
      try {
        const response : CheckRequestResponse<S> = await rs.call('ProofWidgets.checkRequest', id)
        if (response === 'running') return
        window.clearInterval(i)
        resolve(response.done.result)
      } catch (e) {
        window.clearInterval(i)
        reject(e)
      }
    }, 100)
    cancelFn.fn = () => {
      void rs.call('ProofWidgets.cancelRequest', id)
    }
  })
  return [promise, cancelFn]
}
