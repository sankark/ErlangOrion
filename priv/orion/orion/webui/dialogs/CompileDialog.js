define(
		[ 'i18n!orion/nls/messages', 'orion/webui/dialog' ],
		function(messages, dialog) {

			function CompileDialog(options) {
				this._init(options);
			}

			CompileDialog.prototype = new dialog.Dialog();

			CompileDialog.prototype.TEMPLATE = '<textarea id="commitMessage" cols="100" rows="5" readonly></textarea>';
					

			CompileDialog.prototype._init = function(options) {
				var that = this;

				this.title = messages["Commit Changes"];
				this.modal = true;
				this.messages = messages;
				this.options = options;

				this.buttons = [];

				this.buttons.push({ callback : function() {
					that._execute();
				},
				text : 'OK',
				id : 'ok'
				});

				// Start the dialog initialization.
				this._initialize();
			};

			CompileDialog.prototype._bindToDom = function(parent) {
				var that = this;

				if (this.options.body) {
					this.$commitMessage.value = this.options.body;
				}
				
			};

				CompileDialog.prototype._execute = function() {
				
				this.hide();
			};

			CompileDialog.prototype.constructor = CompileDialog;

			// return the module exports
			return { CompileDialog : CompileDialog
			};

		});