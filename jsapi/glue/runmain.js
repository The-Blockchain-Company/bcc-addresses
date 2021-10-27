function runBccAddressesApi(bccAddressesInitComplete) {
  // TODO find a way to avoid passing this using globalThis
  globalThis.bccAddressesInitComplete = bccAddressesInitComplete;
  // Prevent exiting the process on cleanup.
  h$exitProcess = stopBccAddressesApi;
  // Start
  h$main(h$mainZCZCMainzimain);
}

function stopBccAddressesApi(code) {
  if(h$currentThread) {
    h$finishThread(h$currentThread);
    h$stack = null;
    throw new h$ThreadAbortedError(code);
  }
}
