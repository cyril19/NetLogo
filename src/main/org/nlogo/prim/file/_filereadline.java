package org.nlogo.prim.file;

import org.nlogo.api.LogoException;
import org.nlogo.nvm.EngineException;
import org.nlogo.nvm.Reporter;
import org.nlogo.api.Syntax;

public final strictfp class _filereadline
    extends Reporter {
  @Override
  public Object report(final org.nlogo.nvm.Context context) throws LogoException {
    try {
      return workspace.fileManager().readLine();
    } catch (java.io.EOFException ex) {
      throw new EngineException(context, this, "The end of file has been reached");
    } catch (java.io.IOException ex) {
      throw new EngineException(context, this, ex.getMessage());
    }
  }

  @Override
  public Syntax syntax() {
    int[] right = {};
    int ret = Syntax.StringType();
    return Syntax.reporterSyntax(right, ret);
  }
}