package org.nlogo.prim.file;

import org.nlogo.api.LogoException;
import org.nlogo.nvm.EngineException;
import org.nlogo.api.Syntax;
import org.nlogo.nvm.Workspace;

public final strictfp class _filetype
    extends org.nlogo.nvm.Command {
  @Override
  public void perform(final org.nlogo.nvm.Context context)
      throws LogoException {
    try {
      workspace.fileManager().ensureMode
         (org.nlogo.api.FileModeJ.APPEND());
    } catch (java.io.IOException ex) {
      throw new EngineException(context, this, ex.getMessage());
    }
    workspace.outputObject
        (args[0].report(context),
            null, false, false,
            Workspace.OutputDestination.FILE);
    context.ip = next;
  }

  @Override
  public Syntax syntax() {
    int[] right = {Syntax.WildcardType()};
    return Syntax.commandSyntax(right);
  }
}